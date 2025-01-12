{-# LANGUAGE OverloadedStrings #-}

module TestLInt (tests) where

import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck

import Core (renderText)
import LInt

instance Arbitrary Expr where
  -- NOTE: Generation of @Prim Read []@ is excluded because it makes tests halt.
  arbitrary = sized \n ->
    if n <= 0
      then Lit <$> arbitrary
      else
        oneof
          [ Lit <$> arbitrary
          , neg <$> resize (n `div` 2) arbitrary
          , do
              a <- resize (n `div` 2) arbitrary
              b <- resize (n `div` 2) arbitrary
              elements [add a b, sub a b]
          ]

checkPE :: Expr -> Assertion
checkPE e = do
  ir <- interpExpr e
  pr <- interpExpr (peExpr e)
  ir @?= pr

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
    , testProperty "partial evaluation does not change behavior" $ property \e -> monadicIO do
        ir <- run (interpExpr e)
        pr <- run (interpExpr (peExpr e))
        assert (ir == pr)
    ]

groupPretty :: TestTree
groupPretty =
  testGroup
    "Pretty printer"
    [ testCase "lit" do
        "42" @?= (renderText (Lit 42))
    , testCase "read" do
        "(read)" @?= (renderText read_)
    , testCase "neg" do
        "(- 42)" @?= (renderText (neg (Lit 42)))
    , testCase "add" do
        "(+ 32 10)" @?= (renderText (add (Lit 32) (Lit 10)))
    , testCase "sub" do
        "(- 32 10)" @?= (renderText (sub (Lit 32) (Lit 10)))
    , testCase "arith" do
        "(+ (- 42 10) (- (- 10)))" @?= (renderText (add (sub (Lit 42) (Lit 10)) (neg (neg (Lit 10)))))
    ]
