module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import TestCVar qualified
import TestLInt qualified
import TestLVar qualified
import TestPipeline qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [TestLInt.tests, TestLVar.tests, TestCVar.tests, TestPipeline.tests]
