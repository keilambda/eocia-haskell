module X86 (Reg (..), InstrF (..)) where

import Data.Kind (Type)

type Reg :: Type
data Reg = RSP | RBP | RAX | RBX | RCX | RDX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

instance Show Reg where
  show = \case
    RSP -> "rsp"
    RBP -> "rbp"
    RAX -> "rax"
    RBX -> "rbx"
    RCX -> "rcx"
    RDX -> "rdx"
    RSI -> "rsi"
    RDI -> "rdi"
    R8 -> "r8"
    R9 -> "r9"
    R10 -> "r10"
    R11 -> "r11"
    R12 -> "r12"
    R13 -> "r13"
    R14 -> "r14"
    R15 -> "r15"

type role InstrF representational
type InstrF :: Type -> Type
data InstrF arg
  = AddQ arg arg
  | SubQ arg arg
  | NegQ arg
  | MovQ arg arg
  | PushQ arg
  | PopQ arg
  | CallQ String Int
  | Jmp String
  | Syscall
  | RetQ

instance (Show arg) => Show (InstrF arg) where
  show = \case
    AddQ src dst -> "addq " ++ show src ++ ", " ++ show dst
    SubQ src dst -> "subq " ++ show src ++ ", " ++ show dst
    NegQ arg -> "negq " ++ show arg
    MovQ src dst -> "movq " ++ show src ++ ", " ++ show dst
    PushQ arg -> "pushq " ++ show arg
    PopQ arg -> "popq " ++ show arg
    CallQ lbl n -> "callq " ++ show lbl ++ ", " ++ show n
    Jmp lbl -> "jmp " ++ show lbl
    Syscall -> "syscall"
    RetQ -> "retq"
