{-# LANGUAGE BlockArguments #-}

module TestLInt (module TestLInt) where

import Test.QuickCheck.Monadic
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck

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

unit_1 :: Assertion
unit_1 = checkPE (add (Lit 10) (neg (add (Lit 5) (Lit 3))))

unit_2 :: Assertion
unit_2 = checkPE (add (Lit 1) (add (Lit 3) (Lit 1)))

unit_3 :: Assertion
unit_3 = checkPE (neg (add (Lit 3) (neg (Lit 5))))

prop_peCorrect :: Expr -> Property
prop_peCorrect e = monadicIO do
  ir <- run (interpExpr e)
  pr <- run (interpExpr (peExpr e))
  assert (ir == pr)
