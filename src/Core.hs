module Core
  ( Name (MkName, getName)
  , Label (MkLabel, getLabel)
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Text (Text)

import Prettyprinter (Pretty (pretty))

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
