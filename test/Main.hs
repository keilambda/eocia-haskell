module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import TestLInt qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [TestLInt.tests]
