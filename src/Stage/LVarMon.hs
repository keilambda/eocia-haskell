module Stage.LVarMon (module Stage.LVarMon) where

import Data.Kind (Type)

import Prettyprinter

import Core (Atom (..), BinOp (..), Name, NulOp (..), UnOp (..))

type Expr :: Type
data Expr
  = Atom Atom
  | Let Name Expr Expr
  | NulApp NulOp
  | UnApp UnOp Atom
  | BinApp BinOp Atom Atom
  deriving stock (Show, Eq)

lit :: Int -> Expr
lit = Atom . Lit

var :: Name -> Expr
var = Atom . Var

read_ :: Expr
read_ = NulApp Read

neg :: Atom -> Expr
neg a = UnApp Neg a

add :: Atom -> Atom -> Expr
add a b = BinApp Add a b

sub :: Atom -> Atom -> Expr
sub a b = BinApp Sub a b

instance Pretty Expr where
  pretty = \case
    Atom a -> pretty a
    Let n e b -> parens $ pretty "let" <+> brackets (pretty n <+> pretty e) <+> pretty b
    NulApp op -> pretty op
    UnApp op a -> parens $ pretty op <+> pretty a
    BinApp op a b -> parens $ pretty op <+> pretty a <+> pretty b
