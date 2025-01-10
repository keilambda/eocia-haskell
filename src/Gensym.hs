module Gensym (MonadGensym (gensym)) where

import Control.Monad.State (State, get, modify)
import Data.Kind (Constraint, Type)
import Data.Unique (hashUnique, newUnique)

type MonadGensym :: (Type -> Type) -> Constraint
class (Monad m) => MonadGensym m where
  gensym :: String -> m String

instance MonadGensym (State Int) where
  gensym p = do
    n <- get
    modify (+ 1)
    pure (p ++ show n)

instance MonadGensym IO where
  gensym p = do
    n <- newUnique
    pure (p ++ show (hashUnique n))
