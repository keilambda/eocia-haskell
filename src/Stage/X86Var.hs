module Stage.X86Var (module Stage.X86Var) where

import Data.Kind (Type)
import Data.List (List)

import Prettyprinter

import Core (Name)
import Stage.X86 (InstrF, Reg)

type Arg :: Type
data Arg
  = Imm Int
  | Reg Reg
  | Deref Int Reg
  | Var Name
  deriving stock (Show)

instance Pretty Arg where
  pretty = \case
    Imm n -> pretty "$" <> pretty n
    Reg r -> pretty "%" <> pretty r
    Deref n r -> pretty n <> parens (pretty "%" <> pretty r)
    Var n -> pretty n

type Instr :: Type
type Instr = InstrF Arg

type Block :: Type
newtype Block = MkBlock {getBlock :: List Instr}
  deriving stock (Show)

instance Pretty Block where
  pretty (MkBlock xs) = vsep (map (align . pretty) xs)
