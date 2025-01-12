module Core
  ( Name (MkName, getName)
  , Label (MkLabel, getLabel)
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Type)

type Name :: Type
newtype Name = MkName {getName :: String}
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

type Label :: Type
newtype Label = MkLabel {getLabel :: String}
  deriving stock (Show, Eq)
  deriving newtype (Hashable)
