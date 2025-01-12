module Core
  ( Name (MkName, getName)
  , Label (MkLabel, getLabel)
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Text (Text)

type Name :: Type
newtype Name = MkName {getName :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

type Label :: Type
newtype Label = MkLabel {getLabel :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Hashable)
