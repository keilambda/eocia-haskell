module TestPipeline (tests) where

import Control.Monad.State (evalState)

import Data.HashMap.Strict (HashMap, insertWith, unionWith)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Arbitrary ()
import Core (Name, renderText)
import Pipeline (passRemoveComplexOperands, passUniquify)
import Stage.CVar qualified as CVar
import Stage.LVar qualified as LVar
import Stage.LVarMon qualified as LVarMon

tests :: TestTree
tests = testGroup "Pipeline" [groupPassUniquify, groupPassRemoveComplexOperands]

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
    [ testProperty "variable is bound at most once" \expr ->
        let vars = countVars (evalState (passUniquify expr) (0 :: Int))
         in all (<= 1) vars
    , testProperty "preserves semantics" \expr -> ioProperty do
        orig <- LVar.runInterpExpr expr
        uniq <- LVar.runInterpExpr (evalState (passUniquify expr) (0 :: Int))
        pure $ orig == uniq
    , testCase "preserves semantics" $
        let expr = LVar.Let "x" (LVar.Lit 1) (LVar.Let "x" (LVar.Var "x") (LVar.Var "x"))
            expr' = evalState (passUniquify expr) (0 :: Int)
         in renderText expr' @?= "(let [x.0 1] (let [x.1 x.0] x.1))"
    ]

groupPassRemoveComplexOperands :: TestTree
groupPassRemoveComplexOperands =
  testGroup
    "passRemoveComplexOperands"
    [ testCase "breaks down complex operands into monadic form" $
        let expr = LVar.Let "x" (LVar.add (LVar.Lit 42) (LVar.neg (LVar.Lit 10))) (LVar.add (LVar.Var "x") (LVar.Lit 10))
         in evalState (passRemoveComplexOperands expr) (1 :: Int)
              @?= LVarMon.Let
                "x"
                (LVarMon.Let "tmp.1" (LVarMon.neg (CVar.Lit 10)) (LVarMon.add (CVar.Lit 42) (CVar.Var "tmp.1")))
                (LVarMon.add (CVar.Var "x") (CVar.Lit 10))
    , testCase "does not simplify already simple expression" $
        let expr = LVar.Let "a" (LVar.Lit 42) (LVar.Let "b" (LVar.Var "a") (LVar.Var "b"))
         in evalState (passRemoveComplexOperands expr) (1 :: Int)
              @?= LVarMon.Let "a" (LVarMon.lit 42) (LVarMon.Let "b" (LVarMon.var "a") (LVarMon.var "b"))
    ]
