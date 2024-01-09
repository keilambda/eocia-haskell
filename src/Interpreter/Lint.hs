{-# LANGUAGE NoImplicitPrelude #-}

module Interpreter.Lint (module Interpreter.Lint) where

import Data.Kind (Type)
import Prelude (Bool (..), IO, Show (show), all, error, pure, (+), (++), (-), (<$>), (<*>))
import Prelude qualified as P

type Op :: Type
data Op = Read | Negate | Minus | Plus
  deriving stock (Show)

type Exp :: Type
data Exp
  = Int P.Int
  | Prim Op [Exp]
  deriving stock (Show)

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

isLeaf :: Exp -> Bool
isLeaf = \case
  Int _ -> True
  Prim Read _ -> True
  _ -> False

isExp :: Exp -> Bool
isExp = \case
  Int _ -> True
  Prim Read _ -> True
  Prim _ args -> all isExp args

isLint :: Program info -> Bool
isLint (MkProgram _ e) = isExp e

interpExp :: Exp -> IO P.Int
interpExp = \case
  Int n -> pure n
  Prim Read [] -> P.read <$> P.getLine
  Prim Negate [a] -> P.negate <$> interpExp a
  Prim Minus [a, b] -> (-) <$> interpExp a <*> interpExp b
  Prim Plus [a, b] -> (+) <$> interpExp a <*> interpExp b
  e -> error ("Invalid expression: " ++ show e)

interpLint :: Program info -> IO P.Int
interpLint (MkProgram _ e) = interpExp e
