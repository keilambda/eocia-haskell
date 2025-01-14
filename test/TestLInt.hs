module TestLInt (tests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck

import Arbitrary ()
import Core (renderText)
import Stage.LInt

checkPE :: Expr -> Assertion
checkPE e = do
  ir <- interpExpr e
  pr <- interpExpr (peExpr e)
  pr @?= ir

tests :: TestTree
tests = testGroup "LInt" [groupPartialEvaluation, groupPretty]

groupPartialEvaluation :: TestTree
groupPartialEvaluation =
  testGroup
    "Partial evaluation"
    [ testCase "simple arithmetic" do
        checkPE (add (Lit 10) (neg (add (Lit 5) (Lit 3))))
        checkPE (add (Lit 1) (add (Lit 3) (Lit 1)))
        checkPE (neg (add (Lit 3) (neg (Lit 5))))
    , testProperty "partial evaluation does not change behavior" \e -> ioProperty do
        ir <- interpExpr e
        pr <- interpExpr (peExpr e)
        pure (ir == pr)
    ]

groupPretty :: TestTree
groupPretty =
  testGroup
    "Pretty printer"
    [ testCase "lit" do
        renderText (Lit 42) @?= "42"
    , testCase "read" do
        renderText read_ @?= "(read)"
    , testCase "neg" do
        renderText (neg (Lit 42)) @?= "(- 42)"
    , testCase "add" do
        renderText (add (Lit 32) (Lit 10)) @?= "(+ 32 10)"
    , testCase "sub" do
        renderText (sub (Lit 32) (Lit 10)) @?= "(- 32 10)"
    , testCase "arith" do
        renderText (add (sub (Lit 42) (Lit 10)) (neg (neg (Lit 10)))) @?= "(+ (- 42 10) (- (- 10)))"
    ]
