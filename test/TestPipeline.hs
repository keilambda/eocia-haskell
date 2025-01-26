module TestPipeline (tests) where

import Control.Monad.State (evalState)

import Data.HashMap.Strict (HashMap, insertWith, unionWith)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Arbitrary ()
import Core (Name, renderText)
import Pipeline (passExplicateControl, passRemoveComplexOperands, passSelectInstructions, passUniquify)
import Stage.CVar qualified as CVar
import Stage.LInt (BinOp (Add, Sub), UnOp (Neg))
import Stage.LVar qualified as LVar
import Stage.LVarMon qualified as LVarMon
import Stage.X86 (InstrF (..), Reg (RAX))
import Stage.X86Var qualified as X86Var

tests :: TestTree
tests =
  testGroup
    "Pipeline"
    [ groupPassUniquify
    , groupPassRemoveComplexOperands
    , groupPassExplicateControl
    , groupPassSelectInstructions
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
        pure $ orig == uniq
    , testCase "preserves semantics" do
        let expr = LVar.Let "x" (LVar.Lit 1) (LVar.Let "x" (LVar.Var "x") (LVar.Var "x"))
            expr' = evalState (passUniquify expr) (0 :: Int)
        renderText expr' @?= "(let [x.0 1] (let [x.1 x.0] x.1))"
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
            (LVarMon.Let "tmp.1" (LVarMon.neg (CVar.Lit 10)) (LVarMon.add (CVar.Lit 42) (CVar.Var "tmp.1")))
            (LVarMon.add (CVar.Var "x") (CVar.Lit 10))
    , testCase "does not simplify already simple expression" do
        let expr = LVar.Let "a" (LVar.Lit 42) (LVar.Let "b" (LVar.Var "a") (LVar.Var "b"))
        evalState (passRemoveComplexOperands expr) (1 :: Int)
          @?= LVarMon.Let "a" (LVarMon.lit 42) (LVarMon.Let "b" (LVarMon.var "a") (LVarMon.var "b"))
    ]

groupPassExplicateControl :: TestTree
groupPassExplicateControl =
  testGroup
    "passExplicateControl"
    [ testCase "converts expressions into statements ending with a return" do
        let let_ = LVarMon.Let
            lit = LVarMon.lit
            var = CVar.Var
            clit = CVar.Atom . CVar.Lit
            expr = let_ "y" (let_ "x.1" (lit 20) (let_ "x.2" (lit 22) (LVarMon.BinApp Add (var "x.1") (var "x.2")))) (LVarMon.var "y")
        passExplicateControl expr
          @?= CVar.Seq
            (CVar.Assign "x.1" (clit 20))
            ( CVar.Seq
                (CVar.Assign "x.2" (clit 22))
                ( CVar.Seq
                    (CVar.Assign "y" (CVar.BinApp Add (var "x.1") (var "x.2")))
                    (CVar.Return (CVar.Atom (var "y")))
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
                (CVar.Assign "x" (CVar.BinApp Add (CVar.Lit 32) (CVar.Lit 10)))
                (CVar.Seq (CVar.Assign "y" (CVar.UnApp Neg (CVar.Var "x"))) (CVar.Return (CVar.Atom (CVar.Var "y"))))
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
            , Jmp "conclusion"
            ]
    , testProperty "always ends with jmp to conclusion" \e -> do
        let (X86Var.MkBlock instr) = passSelectInstructions e
        last instr == Jmp "conclusion"
    , testCase "optimizes compound assignment (left)" do
        let expr =
              CVar.Seq
                (CVar.Assign "x" (CVar.BinApp Add (CVar.Var "x") (CVar.Lit 42)))
                (CVar.Return (CVar.Atom (CVar.Var "x")))
        passSelectInstructions expr
          @?= X86Var.MkBlock
            [ AddQ (imm 42) (var "x")
            , MovQ (var "x") (reg RAX)
            , Jmp "conclusion"
            ]
    , testCase "optimizes compound assignment (right)" do
        let expr =
              CVar.Seq
                (CVar.Assign "y" (CVar.BinApp Add (CVar.Lit 42) (CVar.Var "y")))
                (CVar.Return (CVar.Atom (CVar.Var "y")))
        passSelectInstructions expr
          @?= X86Var.MkBlock
            [ AddQ (imm 42) (var "y")
            , MovQ (var "y") (reg RAX)
            , Jmp "conclusion"
            ]
    , testProperty "distinct operands optimization" $ forAll
        (arbitrary `suchThat` \(_, a, b) -> let cvar = CVar.Var "x" in cvar /= a && cvar /= b)
        \(op, atma, atmb) -> do
          let expr =
                CVar.Seq
                  (CVar.Assign "x" (CVar.BinApp op atma atmb))
                  (CVar.Return (CVar.Atom (CVar.Var "x")))
          passSelectInstructions expr
            == X86Var.MkBlock
              [ MovQ (fromAtom atma) (var "x")
              , (fromOp op) (fromAtom atmb) (var "x")
              , MovQ (var "x") (reg RAX)
              , Jmp "conclusion"
              ]
    ]
 where
  imm = X86Var.Imm
  reg = X86Var.Reg
  var = X86Var.Var
  fromAtom = \case
    CVar.Lit a -> X86Var.Imm a
    CVar.Var a -> X86Var.Var a
  fromOp = \case
    Add -> AddQ
    Sub -> SubQ
