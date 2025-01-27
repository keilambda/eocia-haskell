module Core.Gensym (MonadGensym (gensym)) where

import Control.Monad.State.Strict (State, get, put)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Unique (hashUnique, newUnique)

type MonadGensym :: (Type -> Type) -> Constraint
class (Monad m) => MonadGensym m where
  gensym :: Text -> m Text

instance (Num a, Show a) => MonadGensym (State a) where
  gensym p = do
    n <- get
    put (n + 1)
    pure (p <> T.show n)

instance MonadGensym IO where
  gensym p = do
    n <- newUnique
    pure (p <> T.show (hashUnique n))
