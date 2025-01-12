module TestLVar (tests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck

import Arbitrary ()
import Core (renderText)
import Stage.LInt (Op (..))
import Stage.LVar

instance Arbitrary Expr where
  arbitrary = sized \n ->
    if n <= 0
      then oneof [Lit <$> arbitrary, Var <$> arbitrary]
      else
        oneof
          [ Lit <$> arbitrary
          , Var <$> arbitrary
          , Let <$> arbitrary <*> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
          , neg <$> resize (n `div` 2) arbitrary
          , do
              a <- resize (n `div` 2) arbitrary
              b <- resize (n `div` 2) arbitrary
              elements [add a b, sub a b]
          ]

shouldEvalTo :: Expr -> Either LVarErr Int -> IO ()
shouldEvalTo expr expected = do
  res <- runInterpExpr expr
  res @?= expected

tests :: TestTree
tests = testGroup "LVar" [groupInterpreter, groupErrors, groupPretty]

groupInterpreter :: TestTree
groupInterpreter =
  testGroup
    "Interpreter"
    [ testCase "lit" do
        (Lit 42) `shouldEvalTo` (Right 42)
    , testCase "arithmetic" do
        (add (Lit 3) (Lit 2)) `shouldEvalTo` (Right 5)
        (sub (Lit 3) (Lit 2)) `shouldEvalTo` (Right 1)
        (neg (Lit 5)) `shouldEvalTo` (Right (-5))
    , groupLet
    ]

groupLet :: TestTree
groupLet =
  testGroup
    "Let"
    [ testCase "let" do
        (Let "x" (Lit 42) (Var "x")) `shouldEvalTo` (Right 42)
    , testCase "nested let" do
        (Let "x" (Lit 32) (Let "y" (add (Var "x") (Lit 10)) (Var "y"))) `shouldEvalTo` (Right 42)
    , testCase "shadowed let" do
        (Let "x" (Lit 0) (Let "x" (Lit 42) (Var "x"))) `shouldEvalTo` (Right 42)
    , testCase "nested and shadowed let; let is not recursive" do
        (Let "x" (Lit 42) (Let "x" (Var "x") (Var "x"))) `shouldEvalTo` (Right 42)
    ]

groupErrors :: TestTree
groupErrors =
  testGroup
    "Errors"
    [ testCase "unbound variable" do
        (Var "x") `shouldEvalTo` (Left (UnboundVariable "x"))
    , testCase "malformed expression" do
        let expr = Prim Add [Lit 1]
        expr `shouldEvalTo` (Left (BadSpecialForm expr))
    ]

groupPretty :: TestTree
groupPretty =
  testGroup
    "Pretty printer"
    [ testCase "let expression" do
        renderText (Let "x" (Lit 42) (Var "x")) @?= "(let [x 42] x)"
    , testCase "complex let expression" do
        let expr = Let "x" (add (Lit 1) (Lit 2)) (Let "y" (sub (Var "x") (Lit 1)) (add (Var "x") (Var "y")))
        renderText expr @?= "(let [x (+ 1 2)] (let [y (- x 1)] (+ x y)))"
    ]
