module TestCVar (tests) where

import Core (Atom (..), BinOp (..), UnOp (..), aint)
import Pre
import Stage.CVar
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CVar" [groupPretty]

groupPretty :: TestTree
groupPretty = testGroup "Pretty printer" [groupPrettyTail]

groupPrettyTail :: TestTree
groupPrettyTail =
  testGroup
    "Tail"
    [ testCase "return" do
        renderText (Return (Atom (aint 42))) @?= "return 42;"
    , testCase "assignment" do
        renderText (Assign "x" (Atom (aint 42))) @?= "x = 42;"
    , testCase "sequence binop" do
        let stmt = Assign "x" (BinApp Add (aint 1) (aint 2))
            rest = Return (Atom (Var "x"))
        renderText (Seq stmt rest) @?= "x = 1 + 2;\nreturn x;"
    , testCase "sequence unop" do
        let stmt = Assign "x" (UnApp Neg (aint 1))
            rest = Return (Atom (Var "x"))
        renderText (Seq stmt rest) @?= "x = - 1;\nreturn x;"
    ]
