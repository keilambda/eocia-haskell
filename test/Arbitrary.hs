module Arbitrary () where

import Data.Text (pack)

import Test.Tasty.QuickCheck

import Core (Label (MkLabel), Name (MkName))
import Stage.LInt qualified as LInt
import Stage.LVar qualified as LVar

instance Arbitrary Name where
  arbitrary = MkName . pack <$> ((:) <$> elements first <*> listOf (elements rest))
   where
    first = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "-_!$%&*+/:<=>?@^~"
    rest = first ++ ['0' .. '9']

instance Arbitrary Label where
  arbitrary = MkLabel . pack <$> ((:) <$> elements first <*> listOf (elements rest))
   where
    first = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_"
    rest = first ++ ['0' .. '9']

instance Arbitrary LInt.Expr where
  -- NOTE: Generation of @Prim Read []@ is excluded because it makes tests halt.
  arbitrary = sized \n ->
    if n <= 0
      then LInt.Lit <$> arbitrary
      else
        oneof
          [ LInt.Lit <$> arbitrary
          , LInt.neg <$> resize (n `div` 2) arbitrary
          , do
              a <- resize (n `div` 2) arbitrary
              b <- resize (n `div` 2) arbitrary
              elements [LInt.add a b, LInt.sub a b]
          ]

instance Arbitrary LVar.Expr where
  arbitrary = sized \n ->
    if n <= 0
      then oneof [LVar.Lit <$> arbitrary, LVar.Var <$> arbitrary]
      else
        oneof
          [ LVar.Lit <$> arbitrary
          , LVar.Var <$> arbitrary
          , LVar.Let <$> arbitrary <*> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
          , LVar.neg <$> resize (n `div` 2) arbitrary
          , do
              a <- resize (n `div` 2) arbitrary
              b <- resize (n `div` 2) arbitrary
              elements [LVar.add a b, LVar.sub a b]
          ]
