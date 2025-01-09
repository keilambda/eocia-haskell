{-# LANGUAGE QuasiQuotes #-}

module CVar (module CVar) where

import Data.HashMap.Strict (HashMap)
import Data.Kind (Type)
import PyF (fmt)

type Atom :: Type
data Atom = Lit Int | Var String
  deriving stock (Show)

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
  deriving stock (Show)

type Stmt :: Type
data Stmt = Assign String Expr

instance Show Stmt where
  show (Assign n e) = [fmt|{n} = {show e}|]

type Tail :: Type
data Tail = Return Expr | Seq Stmt Tail

instance Show Tail where
  show = \case
    Return e -> [fmt|return {show e};|]
    Seq s t -> [fmt|{show s}\n{show t}|]

type Program :: Type
data Program = MkProgram {env :: HashMap String Expr, blocks :: HashMap String Tail}
