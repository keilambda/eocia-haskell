{-# LANGUAGE QuasiQuotes #-}

module CVar (module CVar) where

import Data.HashMap.Strict (HashMap)
import Data.Kind (Type)
import PyF (fmt)

import Core (Name (MkName))

type Atom :: Type
data Atom = Lit Int | Var Name

instance Show Atom where
  show = \case
    Lit n -> show n
    Var (MkName n) -> show n

type Op :: Type
data Op = Read | Neg Atom | Add Atom Atom | Sub Atom Atom

instance Show Op where
  show = \case
    Read -> "(read)"
    Neg a -> [fmt|(- {show a})|]
    Add a b -> [fmt|(+ {show a} {show b})|]
    Sub a b -> [fmt|(- {show a} {show b})|]

type Expr :: Type
data Expr
  = Atom Atom
  | Prim Op

instance Show Expr where
  show = \case
    Atom a -> show a
    Prim op -> show op

type Stmt :: Type
data Stmt = Assign Name Expr

instance Show Stmt where
  show (Assign (MkName n) e) = [fmt|{n} = {show e}|]

type Tail :: Type
data Tail = Return Expr | Seq Stmt Tail

instance Show Tail where
  show = \case
    Return e -> [fmt|return {show e};|]
    Seq s t -> [fmt|{show s}\n{show t}|]

type Program :: Type
data Program = MkProgram {env :: HashMap Name Expr, blocks :: HashMap Name Tail}
