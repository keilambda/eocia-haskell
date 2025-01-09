module X86 (Reg (..)) where

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
