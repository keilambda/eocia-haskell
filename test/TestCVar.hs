module TestCVar (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Core (renderText)
import Stage.CVar

tests :: TestTree
tests = testGroup "CVar" [groupPretty]

groupPretty :: TestTree
groupPretty = testGroup "Pretty printer" [groupPrettyTail]

groupPrettyTail :: TestTree
groupPrettyTail =
  testGroup
    "Tail"
    [ testCase "return" do
        renderText (Return (Atom (Lit 42))) @?= "return 42;"
    , testCase "assignment" do
        renderText (Assign "x" (Atom (Lit 42))) @?= "x = 42;"
    , testCase "sequence" do
        let stmt = Assign "x" (Prim $ Add (Lit 1) (Lit 2))
            rest = Return (Atom (Var "x"))
        renderText (Seq stmt rest) @?= "x = (+ 1 2);\nreturn x;"
    ]
