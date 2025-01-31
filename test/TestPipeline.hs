module TestPipeline (tests) where

import Control.Monad.State.Strict (evalState)

import Data.HashMap.Strict (HashMap, fromList, insertWith, unionWith)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Arbitrary ()
import Core
import Pipeline
import Stage.CVar qualified as CVar
import Stage.LVar qualified as LVar
import Stage.LVarMon qualified as LVarMon
import Stage.X86Int qualified as X86Int
import Stage.X86Var qualified as X86Var

tests :: TestTree
tests =
  testGroup
    "Pipeline"
    [ groupPassUniquify
    , groupPassRemoveComplexOperands
    , groupPassExplicateControl
    , groupPassSelectInstructions
    , groupPassAssignHomes
    , groupPassPatchInstructions
    , groupPassGeneratePreludeAndConclusion
    ]

countVars :: LVar.Expr -> HashMap Name Int
countVars = \case
  LVar.Lit _ -> mempty
  LVar.Var _ -> mempty
  LVar.Let name expr body -> insertWith (+) name 1 (unionWith (+) (countVars expr) (countVars body))
  LVar.NulApp _ -> mempty
  LVar.UnApp _ a -> countVars a
  LVar.BinApp _ a b -> unionWith (+) (countVars a) (countVars b)

groupPassUniquify :: TestTree
groupPassUniquify =
  testGroup
    "passUniquify"
    [ testProperty "variable is bound at most once" \expr -> do
        let vars = countVars (evalState (passUniquify expr) (0 :: Int))
        all (<= 1) vars
    , testProperty "preserves semantics" \expr -> ioProperty do
        orig <- LVar.runInterpExpr expr
        uniq <- LVar.runInterpExpr (evalState (passUniquify expr) (0 :: Int))
        pure $ case (orig, uniq) of
          (Left (LVar.UnboundVariable _), Left (LVar.UnboundVariable _)) -> True
          _ -> orig == uniq
    , testCase "preserves semantics" do
        let expr = LVar.Let "x" (LVar.Lit 1) (LVar.Let "x" (LVar.Var "x") (LVar.Var "x"))
            expr' = evalState (passUniquify expr) (0 :: Int)
        renderText expr' @?= "(let [x.0 1] (let [x.1 x.0] x.1))"
    , testCase "uniquifies unbound variables" do
        let expr = LVar.Let "x" (LVar.Var "x") (LVar.Var "y")
            expr' = evalState (passUniquify expr) (0 :: Int)
        expr' @?= LVar.Let "x.0" (LVar.Var "x.1") (LVar.Var "y.2")
    , testCase "does not advance the counter on bound variables" do
        let expr = LVar.Let "x" (LVar.Lit 42) $ LVar.Let "x" (LVar.Var "x") (LVar.Var "y")
            expr' = evalState (passUniquify expr) (0 :: Int)
        expr' @?= LVar.Let "x.0" (LVar.Lit 42) (LVar.Let "x.1" (LVar.Var "x.0") (LVar.Var "y.2"))
    ]

groupPassRemoveComplexOperands :: TestTree
groupPassRemoveComplexOperands =
  testGroup
    "passRemoveComplexOperands"
    [ testCase "breaks down complex operands into monadic form" do
        let expr = LVar.Let "x" (LVar.add (LVar.Lit 42) (LVar.neg (LVar.Lit 10))) (LVar.add (LVar.Var "x") (LVar.Lit 10))
        evalState (passRemoveComplexOperands expr) (1 :: Int)
          @?= LVarMon.Let
            "x"
            (LVarMon.Let "tmp.1" (LVarMon.UnApp Neg (Lit 10)) (LVarMon.BinApp Add (Lit 42) (Var "tmp.1")))
            (LVarMon.BinApp Add (Var "x") (Lit 10))
    , testCase "does not simplify already simple expression" do
        let expr = LVar.Let "a" (LVar.Lit 42) (LVar.Let "b" (LVar.Var "a") (LVar.Var "b"))
        evalState (passRemoveComplexOperands expr) (1 :: Int)
          @?= LVarMon.Let "a" (LVarMon.Atom (Lit 42)) (LVarMon.Let "b" (LVarMon.Atom (Var "a")) (LVarMon.Atom (Var "b")))
    ]

groupPassExplicateControl :: TestTree
groupPassExplicateControl =
  testGroup
    "passExplicateControl"
    [ testCase "converts expressions into statements ending with a return" do
        let let_ = LVarMon.Let
            lit = LVarMon.Atom . Lit
            clit = CVar.Atom . Lit
            expr =
              let_
                "y"
                (let_ "x.1" (lit 20) (let_ "x.2" (lit 22) (LVarMon.BinApp Add (Var "x.1") (Var "x.2"))))
                (LVarMon.Atom (Var "y"))
        passExplicateControl expr
          @?= CVar.Seq
            (CVar.Assign "x.1" (clit 20))
            ( CVar.Seq
                (CVar.Assign "x.2" (clit 22))
                ( CVar.Seq
                    (CVar.Assign "y" (CVar.BinApp Add (Var "x.1") (Var "x.2")))
                    (CVar.Return (CVar.Atom (Var "y")))
                )
            )
    ]

groupPassSelectInstructions :: TestTree
groupPassSelectInstructions =
  testGroup
    "passSelectInstructions"
    [ testCase "maps to x86 with vars instructions" do
        let expr =
              CVar.Seq
                (CVar.Assign "x" (CVar.BinApp Add (Lit 32) (Lit 10)))
                (CVar.Seq (CVar.Assign "y" (CVar.UnApp Neg (Var "x"))) (CVar.Return (CVar.Atom (Var "y"))))
        passSelectInstructions expr
          @?= X86Var.MkBlock
            [ -- add
              MovQ (imm 32) (var "x")
            , AddQ (imm 10) (var "x")
            , -- neg
              MovQ (var "x") (reg RAX)
            , NegQ (reg RAX)
            , MovQ (reg RAX) (var "y")
            , -- return
              MovQ (var "y") (reg RAX)
            ]
    , testCase "optimizes compound assignment (left)" do
        let expr =
              CVar.Seq
                (CVar.Assign "x" (CVar.BinApp Add (Var "x") (Lit 42)))
                (CVar.Return (CVar.Atom (Var "x")))
        passSelectInstructions expr
          @?= X86Var.MkBlock
            [ AddQ (imm 42) (var "x")
            , MovQ (var "x") (reg RAX)
            ]
    , testCase "optimizes compound assignment (right)" do
        let expr =
              CVar.Seq
                (CVar.Assign "y" (CVar.BinApp Add (Lit 42) (Var "y")))
                (CVar.Return (CVar.Atom (Var "y")))
        passSelectInstructions expr
          @?= X86Var.MkBlock
            [ AddQ (imm 42) (var "y")
            , MovQ (var "y") (reg RAX)
            ]
    , testProperty "distinct operands optimization" $ forAll
        (arbitrary `suchThat` \(_, a, b) -> let cvar = Var "x" in cvar /= a && cvar /= b)
        \(op, atma, atmb) -> do
          let expr =
                CVar.Seq
                  (CVar.Assign "x" (CVar.BinApp op atma atmb))
                  (CVar.Return (CVar.Atom (Var "x")))
          passSelectInstructions expr
            == X86Var.MkBlock
              [ MovQ (fromAtom atma) (var "x")
              , (fromOp op) (fromAtom atmb) (var "x")
              , MovQ (var "x") (reg RAX)
              ]
    ]
 where
  imm = X86Var.Imm
  reg = X86Var.Reg
  var = X86Var.Var
  fromAtom = \case
    Lit a -> X86Var.Imm a
    Var a -> X86Var.Var a
  fromOp = \case
    Add -> AddQ
    Sub -> SubQ

groupPassAssignHomes :: TestTree
groupPassAssignHomes =
  testGroup
    "passAssignHomes"
    [ testCase "spills variables into stack" do
        let expr =
              [ MovQ (X86Var.Imm 32) (X86Var.Var "x")
              , AddQ (X86Var.Imm 10) (X86Var.Var "x")
              , MovQ (X86Var.Imm 52) (X86Var.Var "y")
              , SubQ (X86Var.Imm 10) (X86Var.Var "y")
              , MovQ (X86Var.Var "y") (X86Var.Reg RAX)
              ]
        evalState (passAssignHomes (X86Var.MkBlock expr)) X86Int.emptyFrame
          @?= X86Int.MkBlock
            [ MovQ (X86Int.Imm 32) (X86Int.Deref (-8) RBP)
            , AddQ (X86Int.Imm 10) (X86Int.Deref (-8) RBP)
            , MovQ (X86Int.Imm 52) (X86Int.Deref (-16) RBP)
            , SubQ (X86Int.Imm 10) (X86Int.Deref (-16) RBP)
            , MovQ (X86Int.Deref (-16) RBP) (X86Int.Reg RAX)
            ]
    ]

groupPassPatchInstructions :: TestTree
groupPassPatchInstructions =
  testGroup
    "passPatchInstructions"
    [ testCase "patches addq double deref instruction" do
        let expr = [AddQ (X86Int.Deref (-8) RBP) (X86Int.Deref (-16) RBP)]
        passPatchInstructions (X86Int.MkBlock expr)
          @?= X86Int.MkBlock
            [ MovQ (X86Int.Deref (-8) RBP) (X86Int.Reg RAX)
            , AddQ (X86Int.Reg RAX) (X86Int.Deref (-16) RBP)
            ]
    , testCase "patches subq double deref instruction" do
        let expr = [SubQ (X86Int.Deref (-8) RBP) (X86Int.Deref (-16) RBP)]
        passPatchInstructions (X86Int.MkBlock expr)
          @?= X86Int.MkBlock
            [ MovQ (X86Int.Deref (-8) RBP) (X86Int.Reg RAX)
            , SubQ (X86Int.Reg RAX) (X86Int.Deref (-16) RBP)
            ]
    , testCase "patches movq double deref instruction" do
        let expr = [MovQ (X86Int.Deref (-8) RBP) (X86Int.Deref (-16) RBP)]
        passPatchInstructions (X86Int.MkBlock expr)
          @?= X86Int.MkBlock
            [ MovQ (X86Int.Deref (-8) RBP) (X86Int.Reg RAX)
            , MovQ (X86Int.Reg RAX) (X86Int.Deref (-16) RBP)
            ]
    ]

groupPassGeneratePreludeAndConclusion :: TestTree
groupPassGeneratePreludeAndConclusion =
  testGroup
    "passGeneratePreludeAndConclusion"
    [ testCase "generates proper prelude and conclusion for Linux" do
        let expr =
              [ MovQ (X86Int.Imm 42) (X86Int.Deref (-8) RBP)
              , MovQ (X86Int.Deref (-8) RBP) (X86Int.Reg RAX)
              , MovQ (X86Int.Reg RAX) (X86Int.Deref (-16) RBP)
              ]
            prelude =
              [ PushQ (X86Int.Reg RBP)
              , MovQ (X86Int.Reg RSP) (X86Int.Reg RBP)
              , SubQ (X86Int.Imm 16) (X86Int.Reg RSP)
              , Jmp "main"
              ]
            main = expr ++ [Jmp "conclusion"]
            conclusion =
              [ AddQ (X86Int.Imm 16) (X86Int.Reg RSP)
              , PopQ (X86Int.Reg RBP)
              , MovQ (X86Int.Imm 60) (X86Int.Reg RAX)
              , MovQ (X86Int.Imm 0) (X86Int.Reg RDI)
              , Syscall
              ]
        passGeneratePreludeAndConclusion Linux 16 (X86Int.MkBlock expr)
          @?= X86Int.MkProgram
            "prelude"
            ( fromList
                [ ("prelude", X86Int.MkBlock prelude)
                , ("main", X86Int.MkBlock main)
                , ("conclusion", X86Int.MkBlock conclusion)
                ]
            )
    , testCase "generates proper prelude and conclusion for Darwin" do
        let expr =
              [ MovQ (X86Int.Imm 42) (X86Int.Deref (-8) RBP)
              , MovQ (X86Int.Deref (-8) RBP) (X86Int.Reg RAX)
              , MovQ (X86Int.Reg RAX) (X86Int.Deref (-16) RBP)
              ]
            prelude =
              [ PushQ (X86Int.Reg RBP)
              , MovQ (X86Int.Reg RSP) (X86Int.Reg RBP)
              , SubQ (X86Int.Imm 16) (X86Int.Reg RSP)
              , Jmp "_main"
              ]
            main = expr ++ [Jmp "_conclusion"]
            conclusion =
              [ AddQ (X86Int.Imm 16) (X86Int.Reg RSP)
              , PopQ (X86Int.Reg RBP)
              , MovQ (X86Int.Imm 0x2000001) (X86Int.Reg RAX)
              , MovQ (X86Int.Imm 0) (X86Int.Reg RDI)
              , Syscall
              ]
        passGeneratePreludeAndConclusion Darwin 16 (X86Int.MkBlock expr)
          @?= X86Int.MkProgram
            "_prelude"
            ( fromList
                [ ("_prelude", X86Int.MkBlock prelude)
                , ("_main", X86Int.MkBlock main)
                , ("_conclusion", X86Int.MkBlock conclusion)
                ]
            )
    ]
