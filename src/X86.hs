module X86 (Reg (..), InstrF (..)) where

import Data.Kind (Type)

import Prettyprinter

import Core (Label)

type Reg :: Type
data Reg = RSP | RBP | RAX | RBX | RCX | RDX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving stock (Show)

instance Pretty Reg where
  pretty = \case
    RSP -> pretty "rsp"
    RBP -> pretty "rbp"
    RAX -> pretty "rax"
    RBX -> pretty "rbx"
    RCX -> pretty "rcx"
    RDX -> pretty "rdx"
    RSI -> pretty "rsi"
    RDI -> pretty "rdi"
    R8 -> pretty "r8"
    R9 -> pretty "r9"
    R10 -> pretty "r10"
    R11 -> pretty "r11"
    R12 -> pretty "r12"
    R13 -> pretty "r13"
    R14 -> pretty "r14"
    R15 -> pretty "r15"

type role InstrF representational
type InstrF :: Type -> Type
data InstrF arg
  = AddQ arg arg
  | SubQ arg arg
  | NegQ arg
  | MovQ arg arg
  | PushQ arg
  | PopQ arg
  | CallQ Label Int
  | Jmp Label
  | Syscall
  | RetQ
  deriving stock (Show)

instance (Pretty arg) => Pretty (InstrF arg) where
  pretty = \case
    AddQ src dst -> pretty "addq" <+> pretty src <> comma <+> pretty dst
    SubQ src dst -> pretty "subq" <+> pretty src <> comma <+> pretty dst
    NegQ arg -> pretty "negq" <+> pretty arg
    MovQ src dst -> pretty "movq" <+> pretty src <> comma <+> pretty dst
    PushQ arg -> pretty "pushq" <+> pretty arg
    PopQ arg -> pretty "popq" <+> pretty arg
    CallQ lbl n -> pretty "callq" <+> pretty lbl <> comma <+> pretty n
    Jmp lbl -> pretty "jmp" <+> pretty lbl
    Syscall -> pretty "syscall"
    RetQ -> pretty "retq"
