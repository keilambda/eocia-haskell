module Core
  ( Name (MkName, getName)
  , Label (MkLabel, getLabel)
  , renderText
  , module Core.Gensym
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)

import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import Core.Gensym

type Name :: Type
newtype Name = MkName {getName :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Hashable, IsString)

instance Pretty Name where
  pretty (MkName t) = pretty t

type Label :: Type
newtype Label = MkLabel {getLabel :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Hashable, IsString)

instance Pretty Label where
  pretty (MkLabel t) = pretty t

renderText :: (Pretty a) => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty
