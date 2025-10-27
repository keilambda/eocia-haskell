module Pre
  ( module Control.Monad
  , module Data.Traversable
  , module Prettyprinter
  , module Prelude
  , module Effectful

    -- * Type in Type
  , Type
  , Constraint

    -- * Data types
  , List
  , HashMap
  , HashSet
  , Text

    -- * Common type classes
  , Generic
  , Hashable

    -- * Pretty
  , renderText
  , putPrettyLn
  )
where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import Data.List (List)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Traversable
import Effectful
import GHC.Generics (Generic)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Prelude

renderText :: (Pretty a) => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty

putPrettyLn :: (Pretty a) => a -> IO ()
putPrettyLn = Text.putStrLn . renderText
