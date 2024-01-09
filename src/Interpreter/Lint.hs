{-# LANGUAGE NoImplicitPrelude #-}

module Interpreter.Lint (module Interpreter.Lint) where

import Data.Kind (Type)
import Prelude qualified as P

type Exp :: Type
data Exp
  = Int P.Int
  | Prim P.String [Exp]

type Program :: Type -> Type
data Program info = MkProgram info Exp
