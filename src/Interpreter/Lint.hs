{-# LANGUAGE NoImplicitPrelude #-}

module Interpreter.Lint (module Interpreter.Lint) where

import Data.Kind (Type)
import Prelude qualified as P

type Op :: Type
data Op = Read | Negate | Minus | Plus

type Exp :: Type
data Exp
  = Int P.Int
  | Prim Op [Exp]

type Program :: Type -> Type
data Program info = MkProgram info Exp

read :: Exp
read = Prim Read []

negate :: Exp -> Exp
negate a = Prim Negate [a]

minus :: Exp -> Exp -> Exp
minus a b = Prim Minus [a, b]

plus :: Exp -> Exp -> Exp
plus a b = Prim Plus [a, b]
