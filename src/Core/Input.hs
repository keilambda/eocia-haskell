{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Core.Input
  ( Input (..)
  , readLine
  , runInputConst
  , runInputStdin
  )
where

import Data.Text.IO qualified as Text
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import Pre

type Input :: Effect
data Input :: Effect where
  ReadLine :: Input m Text

type instance DispatchOf Input = 'Dynamic

makeEffect ''Input

runInputConst :: Text -> Eff (Input ': es) a -> Eff es a
runInputConst input = interpret \_ -> \case
  ReadLine -> pure input

runInputStdin :: (IOE :> es) => Eff (Input ': es) a -> Eff es a
runInputStdin = interpret \_ -> \case
  ReadLine -> liftIO Text.getLine
