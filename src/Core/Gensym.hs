module Core.Gensym (MonadGensym (gensym)) where

import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import Pre

type MonadGensym :: (Type -> Type) -> Constraint
class (Monad m) => MonadGensym m where
  gensym :: Text -> m Text

instance (Num a, Show a) => MonadGensym (State a) where
  gensym p = do
    n <- get
    put (n + 1)
    pure (p <> Text.show n)

instance MonadGensym IO where
  gensym p = do
    n <- newUnique
    pure (p <> Text.show (hashUnique n))
