module Stage.CVar (module Stage.CVar) where

import Data.Kind (Type)

import Prettyprinter

import Core (Atom, BinOp, Name (MkName), NulOp, UnOp)

type Expr :: Type
data Expr
  = Atom Atom
  | NulApp NulOp
  | UnApp UnOp Atom
  | BinApp BinOp Atom Atom
  deriving stock (Show, Eq)

instance Pretty Expr where
  pretty = \case
    Atom a -> pretty a
    NulApp op -> pretty op
    UnApp op a -> pretty op <+> pretty a
    BinApp op a b -> pretty a <+> pretty op <+> pretty b

type Stmt :: Type
data Stmt = Assign Name Expr
  deriving stock (Show, Eq)

instance Pretty Stmt where
  pretty (Assign (MkName n) e) = pretty n <+> equals <+> pretty e <> semi

type Tail :: Type
data Tail = Return Expr | Seq Stmt Tail
  deriving stock (Show, Eq)

instance Pretty Tail where
  pretty = \case
    Return e -> pretty "return" <+> pretty e <> semi
    Seq s t -> pretty s <> hardline <> pretty t
