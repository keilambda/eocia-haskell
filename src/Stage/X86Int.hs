module Stage.X86Int (module Stage.X86Int) where

import Core (InstrF, Label, Name, Reg)
import Data.HashMap.Strict (HashMap, toList)
import Data.Kind (Type)
import Data.List (List)
import GHC.Records (HasField (getField))
import Prettyprinter

type Arg :: Type
data Arg = Imm Int | Reg Reg | Deref Int Reg
  deriving stock (Eq, Show)

instance Pretty Arg where
  pretty = \case
    Imm n -> pretty "$" <> pretty n
    Reg r -> pretty "%" <> pretty r
    Deref n r -> pretty n <> parens (pretty "%" <> pretty r)

type Instr :: Type
type Instr = InstrF Arg

type Frame :: Type
data Frame = MkFrame {env :: HashMap Name Arg, offset :: Int}

emptyFrame :: Frame
emptyFrame = MkFrame{env = mempty, offset = 0}

instance HasField "size" Frame Int where
  getField MkFrame{offset} = let n = negate offset in (n `mod` 16) + n

type Block :: Type
newtype Block = MkBlock (List Instr)
  deriving stock (Show)
  deriving newtype (Eq)

instance Pretty Block where
  pretty (MkBlock xs) = vsep (map (align . pretty) xs)

type Program :: Type
data Program = MkProgram {globl :: Label, blocks :: HashMap Label Block}
  deriving stock (Eq, Show)

instance Pretty Program where
  pretty MkProgram{globl, blocks} =
    pretty ".globl" <+> pretty globl <> hardline <> vsep (map ln (toList blocks)) <> line
   where
    ln (lbl, block) = pretty lbl <> colon <> line <> indent 4 (pretty block)
