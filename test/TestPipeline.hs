module TestPipeline (tests) where

import Control.Monad.State (evalState)

import Data.HashMap.Strict (HashMap, insertWith, unionWith)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Arbitrary ()
import Core (Name, renderText)
import Pipeline (passUniquify)
import Stage.LVar qualified as LVar

tests :: TestTree
tests = testGroup "Pipeline" [groupPassUniquify]

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
