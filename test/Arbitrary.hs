module Arbitrary () where

import Data.Text (pack)

import Test.QuickCheck

import Core (Label (MkLabel), Name (MkName))

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
