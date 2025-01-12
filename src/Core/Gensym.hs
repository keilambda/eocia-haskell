module Core.Gensym (MonadGensym (gensym)) where

import Control.Monad.State (State, get, modify)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Unique (hashUnique, newUnique)

type MonadGensym :: (Type -> Type) -> Constraint
class (Monad m) => MonadGensym m where
  gensym :: Text -> m Text

instance MonadGensym (State Int) where
  gensym p = do
    n <- get
    modify (+ 1)
    pure (p <> T.show n)

instance MonadGensym IO where
  gensym p = do
    n <- newUnique
    pure (p <> T.show (hashUnique n))
