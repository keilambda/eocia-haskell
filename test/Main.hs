module Main (main) where

import Control.Exception (bracket)
import System.IO (hGetBuffering, hSetBuffering, stdout)
import Test.Tasty (TestTree, defaultMain, testGroup)

import TestLInt qualified
import TestLVar qualified
import TestCVar qualified

main :: IO ()
main = protectStdoutBuffering $ defaultMain tests
 where
  protectStdoutBuffering act = bracket (hGetBuffering stdout) (hSetBuffering stdout) (const act)

tests :: TestTree
tests = testGroup "Tests" [TestLInt.tests, TestLVar.tests, TestCVar.tests]
