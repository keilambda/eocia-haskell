module Stage.X86Var (module Stage.X86Var) where

import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List (List)

import GHC.Generics (Generic)

import Prettyprinter

import Core (InstrF, Name, Reg)

type Arg :: Type
data Arg
  = Imm Int
  | Reg Reg
  | Deref Int Reg
  | Var Name
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

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
  deriving newtype (Eq)

instance Pretty Block where
  pretty (MkBlock xs) = vsep (map (align . pretty) xs)
