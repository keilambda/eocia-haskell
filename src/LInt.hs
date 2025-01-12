module LInt (module LInt) where

import Data.Kind (Type)
import Data.List (List)

import Prettyprinter

type Op :: Type
data Op = Read | Neg | Add | Sub
  deriving stock (Show)

instance Pretty Op where
  pretty = \case
    Read -> pretty "read"
    Neg -> pretty "-"
    Add -> pretty "+"
    Sub -> pretty "-"

type Expr :: Type
data Expr
  = Lit Int
  | Prim Op (List Expr)
  deriving stock (Show)

instance Pretty Expr where
  pretty = \case
    Lit n -> pretty n
    Prim op [] -> parens $ pretty op
    Prim op es -> parens $ pretty op <+> hsep (map pretty es)

read_ :: Expr
read_ = Prim Read []

neg :: Expr -> Expr
neg e = Prim Neg [e]

add :: Expr -> Expr -> Expr
add a b = Prim Add [a, b]

sub :: Expr -> Expr -> Expr
sub a b = Prim Sub [a, b]

leaf :: Expr -> Bool
leaf = \case
  Lit _ -> True
  Prim Read [] -> True
  Prim Neg [_] -> False
  Prim Add [_, _] -> False
  Prim Sub [_, _] -> False
  _ -> undefined

-- >>> leaf (Prim Read [])
-- True
-- >>> leaf (Prim Neg [Lit 8])
-- False
-- >>> leaf (Lit 8)
-- True

isExpr :: Expr -> Bool
isExpr = \case
  Lit _ -> True
  Prim Read [] -> True
  Prim Neg [a] -> isExpr a
  Prim Add [a, b] -> (isExpr a) && (isExpr b)
  Prim Sub [a, b] -> (isExpr a) && (isExpr b)
  _ -> False

interpExpr :: Expr -> IO Int
interpExpr = \case
  Lit n -> pure n
  Prim Read [] -> read <$> getLine
  Prim Neg [a] -> negate <$> (interpExpr a)
  Prim Add [a, b] -> (+) <$> (interpExpr a) <*> (interpExpr b)
  Prim Sub [a, b] -> (-) <$> (interpExpr a) <*> (interpExpr b)
  _ -> undefined

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
  Prim Read [] -> read_
  Prim Neg [a] -> peNeg (peExpr a)
  Prim Add [a, b] -> peAdd (peExpr a) (peExpr b)
  Prim Sub [a, b] -> peSub (peExpr a) (peExpr b)
  _ -> undefined
