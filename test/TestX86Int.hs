module TestX86Int (tests) where

import Core (InstrF (..), Reg (..), renderText)
import Data.HashMap.Strict qualified as HashMap
import Stage.X86Int
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "X86Int" [groupPretty]

groupPretty :: TestTree
groupPretty =
  testGroup
    "Pretty printer"
    [ testCase "args" do
        renderText (Imm 42) @?= "$42"
        renderText (Reg RAX) @?= "%rax"
        renderText (Deref 8 RBP) @?= "8(%rbp)"
    , testCase "instructions" do
        renderText (AddQ (Imm 1) (Reg RAX)) @?= "addq $1, %rax"
        renderText (SubQ (Reg RBX) (Reg RAX)) @?= "subq %rbx, %rax"
        renderText (NegQ (Reg RAX)) @?= "negq %rax"
        renderText (MovQ (Imm 42) (Reg RAX)) @?= "movq $42, %rax"
        renderText (PushQ (Reg RBP)) @?= "pushq %rbp"
        renderText (PopQ (Reg RBP)) @?= "popq %rbp"
        renderText (CallQ "printf" 1 :: Instr) @?= "callq printf"
        renderText (Jmp "loop" :: Instr) @?= "jmp loop"
        renderText (Syscall :: Instr) @?= "syscall"
        renderText (RetQ :: Instr) @?= "retq"
    , testCase "program" do
        let block =
              MkBlock
                [ MovQ (Imm 42) (Reg RAX)
                , PushQ (Reg RAX)
                , CallQ "print" 1
                , PopQ (Reg RAX)
                , RetQ
                ]
            prog = MkProgram{globl = "main", blocks = HashMap.fromList [("main", block)]}
        renderText prog
          @?= """
              .globl main
              main:
                  movq $42, %rax
                  pushq %rax
                  callq print
                  popq %rax
                  retq
              """
    ]
