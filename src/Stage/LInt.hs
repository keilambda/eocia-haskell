module Stage.LInt (module Stage.LInt) where

import Core (BinOp (..), NulOp (..), UnOp (..))
import Data.Kind (Type)
import Prettyprinter

type Expr :: Type
data Expr
  = Lit Int
  | NulApp NulOp
  | UnApp UnOp Expr
  | BinApp BinOp Expr Expr
  deriving stock (Show)

instance Pretty Expr where
  pretty = \case
    Lit n -> pretty n
    NulApp op -> parens $ pretty op
    UnApp op arg -> parens $ pretty op <+> pretty arg
    BinApp op hls rhs -> parens $ pretty op <+> pretty hls <+> pretty rhs

read_ :: Expr
read_ = NulApp Read

neg :: Expr -> Expr
neg e = UnApp Neg e

add :: Expr -> Expr -> Expr
add a b = BinApp Add a b

sub :: Expr -> Expr -> Expr
sub a b = BinApp Sub a b

leaf :: Expr -> Bool
leaf = \case
  Lit _ -> True
  NulApp Read -> True
  UnApp Neg _ -> False
  BinApp Add _ _ -> False
  BinApp Sub _ _ -> False

-- >>> leaf (Prim Read [])
-- True
-- >>> leaf (Prim Neg [Lit 8])
-- False
-- >>> leaf (Lit 8)
-- True

isExpr :: Expr -> Bool
isExpr = \case
  Lit _ -> True
  NulApp Read -> True
  UnApp Neg a -> isExpr a
  BinApp Add a b -> isExpr a && isExpr b
  BinApp Sub a b -> isExpr a && isExpr b

interpExpr :: Expr -> IO Int
interpExpr = \case
  Lit n -> pure n
  NulApp Read -> read <$> getLine
  UnApp Neg a -> negate <$> interpExpr a
  BinApp Add a b -> (+) <$> interpExpr a <*> interpExpr b
  BinApp Sub a b -> (-) <$> interpExpr a <*> interpExpr b

peNeg :: Expr -> Expr
peNeg = \case
  Lit n -> Lit (negate n)
  other -> other

peAdd :: Expr -> Expr -> Expr
peAdd = \cases
  (Lit a) (Lit b) -> Lit (a + b)
  a b -> add a b

peSub :: Expr -> Expr -> Expr
peSub = \cases
  (Lit a) (Lit b) -> Lit (a - b)
  a b -> sub a b

peExpr :: Expr -> Expr
peExpr = \case
  Lit n -> Lit n
  NulApp Read -> read_
  UnApp Neg a -> peNeg (peExpr a)
  BinApp Add a b -> peAdd (peExpr a) (peExpr b)
  BinApp Sub a b -> peSub (peExpr a) (peExpr b)
