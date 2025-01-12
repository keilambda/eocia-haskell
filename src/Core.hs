module Core
  ( Name (MkName, getName)
  , Label (MkLabel, getLabel)
  , renderText
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Text (Text)

import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

type Name :: Type
newtype Name = MkName {getName :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

instance Pretty Name where
  pretty (MkName t) = pretty t

type Label :: Type
newtype Label = MkLabel {getLabel :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

instance Pretty Label where
  pretty (MkLabel t) = pretty t

renderText :: (Pretty a) => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty
