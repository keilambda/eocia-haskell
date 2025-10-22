{-# LANGUAGE OverloadedStrings #-}

module Core
  ( Name (MkName, getName)
  , Label (MkLabel, getLabel)
  , lblMain
  , lblPrelude
  , lblConclusion
  , Platform (Linux, Darwin)
  , resolveLabel
  , exitSyscall
  , Atom (Lit, Var)
  , NulOp (Read)
  , UnOp (Neg)
  , BinOp (Add, Sub)
  , Reg (..)
  , InstrF (..)
  , callerSaved
  , calleeSaved
  , argumentPassing
  , module Core.Gensym
  , module Core.Pretty
  )
where

import Core.Gensym
import Core.Pretty
import Data.HashSet qualified as HashSet
import Data.String (IsString)
import Pre

type Name :: Type
newtype Name = MkName {getName :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Hashable, IsString, Ord)

instance Pretty Name where
  pretty (MkName t) = pretty t

type Label :: Type
newtype Label = MkLabel {getLabel :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Hashable, IsString, Semigroup)

instance Pretty Label where
  pretty (MkLabel t) = pretty t

lblPrelude, lblMain, lblConclusion :: Label
lblPrelude = "prelude"
lblMain = "main"
lblConclusion = "conclusion"

type Platform :: Type
data Platform = Linux | Darwin
  deriving stock (Show)

resolveLabel :: Platform -> Label -> Label
resolveLabel = \case
  Darwin -> ("_" <>)
  Linux -> id

exitSyscall :: Platform -> Int
exitSyscall = \case
  Linux -> 60
  Darwin -> 0x2000001

type Atom :: Type
data Atom = Lit Int | Var Name
  deriving stock (Eq, Show)

instance Pretty Atom where
  pretty = \case
    Lit n -> pretty n
    Var n -> pretty n

type NulOp :: Type
data NulOp = Read
  deriving stock (Eq, Show)

instance Pretty NulOp where
  pretty Read = "read"

type UnOp :: Type
data UnOp = Neg
  deriving stock (Eq, Show)

instance Pretty UnOp where
  pretty Neg = "-"

type BinOp :: Type
data BinOp = Add | Sub
  deriving stock (Eq, Show)

instance Pretty BinOp where
  pretty = \case
    Add -> "+"
    Sub -> "-"

type Reg :: Type
data Reg = RSP | RBP | RAX | RBX | RCX | RDX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

instance Pretty Reg where
  pretty = \case
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

callerSaved, calleeSaved, argumentPassing :: List Reg
callerSaved = [RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11]
calleeSaved = [RSP, RBP, RBX, R12, R13, R14, R15]
argumentPassing = [RDI, RSI, RDX, RCX, R8, R9]

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
  deriving stock (Eq, Show)

instance (Pretty arg) => Pretty (InstrF arg) where
  pretty = \case
    AddQ src dst -> "addq" <+> pretty src <> comma <+> pretty dst
    SubQ src dst -> "subq" <+> pretty src <> comma <+> pretty dst
    NegQ arg -> "negq" <+> pretty arg
    MovQ src dst -> "movq" <+> pretty src <> comma <+> pretty dst
    PushQ arg -> "pushq" <+> pretty arg
    PopQ arg -> "popq" <+> pretty arg
    CallQ lbl _ -> "callq" <+> pretty lbl
    Jmp lbl -> "jmp" <+> pretty lbl
    Syscall -> "syscall"
    RetQ -> "retq"

instance (Pretty a) => Pretty (HashSet.HashSet a) where
  pretty = \case
    xs | HashSet.null xs -> "∅"
    xs -> encloseSep "{" "}" ", " (map pretty (HashSet.toList xs))
