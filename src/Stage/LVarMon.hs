module Stage.LVarMon (module Stage.LVarMon) where

import Core (Atom (..), BinOp (..), Name, NulOp (..), UnOp (..))
import Pre

type Expr :: Type
data Expr
  = Atom Atom
  | Let Name Expr Expr
  | NulApp NulOp
  | UnApp UnOp Atom
  | BinApp BinOp Atom Atom
  deriving stock (Eq, Show)

instance Pretty Expr where
  pretty = \case
    Atom a -> pretty a
    Let n e b -> parens $ pretty "let" <+> brackets (pretty n <+> pretty e) <+> pretty b
    NulApp op -> pretty op
    UnApp op a -> parens $ pretty op <+> pretty a
    BinApp op a b -> parens $ pretty op <+> pretty a <+> pretty b
