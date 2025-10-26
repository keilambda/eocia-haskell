{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Core.Gensym
  ( Gensym (..)
  , gensym
  , runGensymPure
  , runGensymIO
  ) where

import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.TH (makeEffect)
import Pre

type Gensym :: Effect
data Gensym :: Effect where
  Gensym :: Text -> Gensym m Text

type instance DispatchOf Gensym = 'Dynamic

makeEffect ''Gensym

runGensymPure :: (Num s, Show s) => s -> Eff (Gensym ': es) a -> Eff es a
runGensymPure s0 = reinterpret (evalState s0) \_ -> \case
  Gensym p -> do
    n <- get
    put (n + 1)
    pure (p <> Text.show n)

runGensymIO :: (IOE :> es) => Eff (Gensym ': es) a -> Eff es a
runGensymIO = interpret \_ -> \case
  Gensym p -> liftIO do
    n <- newUnique
    pure (p <> Text.show (hashUnique n))
