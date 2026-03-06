module TestLVar (tests) where

import Arbitrary ()
import Core (Literal (..))
import Pre
import Stage.LVar
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

shouldEvalTo :: Expr -> Either LVarErr Literal -> IO ()
shouldEvalTo expr expected = do
  res <- runInterpExprIO expr
  res @?= expected

tests :: TestTree
tests = testGroup "LVar" [groupInterpreter, groupErrors, groupPretty]

groupInterpreter :: TestTree
groupInterpreter =
  testGroup
    "Interpreter"
    [ testCase "lit" do
        Lit (LInt 42) `shouldEvalTo` Right (LInt 42)
        Lit (LBool True) `shouldEvalTo` Right (LBool True)
    , testCase "arithmetic" do
        add (Lit (LInt 3)) (Lit (LInt 2)) `shouldEvalTo` Right (LInt 5)
        sub (Lit (LInt 3)) (Lit (LInt 2)) `shouldEvalTo` Right (LInt 1)
        neg (Lit (LInt 5)) `shouldEvalTo` Right (LInt (-5))
    , groupLet
    ]

groupLet :: TestTree
groupLet =
  testGroup
    "Let"
    [ testCase "let" do
        Let "x" (Lit (LInt 42)) (Var "x") `shouldEvalTo` Right (LInt 42)
    , testCase "nested let" do
        Let "x" (Lit (LInt 32)) (Let "y" (add (Var "x") (Lit (LInt 10))) (Var "y")) `shouldEvalTo` Right (LInt 42)
    , testCase "shadowed let" do
        Let "x" (Lit (LInt 0)) (Let "x" (Lit (LInt 42)) (Var "x")) `shouldEvalTo` Right (LInt 42)
    , testCase "nested and shadowed let; let is not recursive" do
        Let "x" (Lit (LInt 42)) (Let "x" (Var "x") (Var "x")) `shouldEvalTo` Right (LInt 42)
    ]

groupErrors :: TestTree
groupErrors =
  testGroup
    "Errors"
    [ testCase "unbound variable" do
        Var "x" `shouldEvalTo` Left (UnboundVariable "x")
    ]

groupPretty :: TestTree
groupPretty =
  testGroup
    "Pretty printer"
    [ testCase "let expression" do
        renderText (Let "x" (Lit (LBool True)) (Var "x")) @?= "(let [x #t] x)"
    , testCase "complex let expression" do
        let expr = Let "x" (add (Lit (LInt 1)) (Lit (LInt 2))) (Let "y" (sub (Var "x") (Lit (LInt 1))) (add (Var "x") (Var "y")))
        renderText expr @?= "(let [x (+ 1 2)] (let [y (- x 1)] (+ x y)))"
    ]
