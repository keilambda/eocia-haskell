module CVar (module CVar) where

import Data.HashMap.Strict (HashMap)
import Data.Kind (Type)

import Prettyprinter

import Core (Name (MkName))

type Atom :: Type
data Atom = Lit Int | Var Name
  deriving stock (Show)

instance Pretty Atom where
  pretty = \case
    Lit n -> pretty n
    Var n -> pretty n

type Op :: Type
data Op = Read | Neg Atom | Add Atom Atom | Sub Atom Atom
  deriving stock (Show)

instance Pretty Op where
  pretty = \case
    Read -> parens $ pretty "read"
    Neg a -> parens $ pretty "-" <+> pretty a
    Add a b -> parens $ pretty "+" <+> pretty a <+> pretty b
    Sub a b -> parens $ pretty "-" <+> pretty a <+> pretty b

type Expr :: Type
data Expr
  = Atom Atom
  | Prim Op
  deriving stock (Show)

instance Pretty Expr where
  pretty = \case
    Atom a -> pretty a
    Prim op -> pretty op

type Stmt :: Type
data Stmt = Assign Name Expr
  deriving stock (Show)

instance Pretty Stmt where
  pretty (Assign (MkName n) e) = pretty n <+> equals <+> pretty e

type Tail :: Type
data Tail = Return Expr | Seq Stmt Tail
  deriving stock (Show)

instance Pretty Tail where
  pretty = \case
    Return e -> pretty "return" <+> pretty e <> semi
    Seq s t -> pretty s <> hardline <> pretty t

type Program :: Type
data Program = MkProgram {env :: HashMap Name Expr, blocks :: HashMap Name Tail}
