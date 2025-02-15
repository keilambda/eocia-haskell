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

import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List (List)
import Data.String (IsString)
import Data.Text (Text, pack)

import GHC.Generics (Generic)

import Prettyprinter

import Core.Gensym
import Core.Pretty

type Name :: Type
newtype Name = MkName {getName :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, IsString)

instance Pretty Name where
  pretty (MkName t) = pretty t

type Label :: Type
newtype Label = MkLabel {getLabel :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Hashable, IsString, Semigroup)

instance Pretty Label where
  pretty (MkLabel t) = pretty t

lblPrelude, lblMain, lblConclusion :: Label
lblPrelude = MkLabel (pack "prelude")
lblMain = MkLabel (pack "main")
lblConclusion = MkLabel (pack "conclusion")

type Platform :: Type
data Platform = Linux | Darwin
  deriving stock (Show)

resolveLabel :: Platform -> Label -> Label
resolveLabel = \case
  Darwin -> (MkLabel (pack "_") <>)
  Linux -> id

exitSyscall :: Platform -> Int
exitSyscall = \case
  Linux -> 60
  Darwin -> 0x2000001

type Atom :: Type
data Atom = Lit Int | Var Name
  deriving stock (Show, Eq)

instance Pretty Atom where
  pretty = \case
    Lit n -> pretty n
    Var n -> pretty n

type NulOp :: Type
data NulOp = Read
  deriving stock (Show, Eq)

instance Pretty NulOp where
  pretty Read = pretty "read"

type UnOp :: Type
data UnOp = Neg
  deriving stock (Show, Eq)

instance Pretty UnOp where
  pretty Neg = pretty "-"

type BinOp :: Type
data BinOp = Add | Sub
  deriving stock (Show, Eq)

instance Pretty BinOp where
  pretty = \case
    Add -> pretty "+"
    Sub -> pretty "-"

type Reg :: Type
data Reg = RSP | RBP | RAX | RBX | RCX | RDX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

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
  deriving stock (Show, Eq)

instance (Pretty arg) => Pretty (InstrF arg) where
  pretty = \case
    AddQ src dst -> pretty "addq" <+> pretty src <> comma <+> pretty dst
    SubQ src dst -> pretty "subq" <+> pretty src <> comma <+> pretty dst
    NegQ arg -> pretty "negq" <+> pretty arg
    MovQ src dst -> pretty "movq" <+> pretty src <> comma <+> pretty dst
    PushQ arg -> pretty "pushq" <+> pretty arg
    PopQ arg -> pretty "popq" <+> pretty arg
    CallQ lbl _ -> pretty "callq" <+> pretty lbl
    Jmp lbl -> pretty "jmp" <+> pretty lbl
    Syscall -> pretty "syscall"
    RetQ -> pretty "retq"

instance (Pretty a) => Pretty (HashSet.HashSet a) where
  pretty = \case
    xs | HashSet.null xs -> pretty "∅"
    xs -> encloseSep (pretty "{") (pretty "}") (pretty ", ") (map pretty (HashSet.toList xs))
