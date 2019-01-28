{-# LANGUAGE DeriveTraversable #-}

module Language.UAST () where

import Data.Text
import Data.List.NonEmpty
import Data.Comp

data Field a = Field {
  _fieldName :: Text,
  _fieldTpe :: a
  } deriving (Foldable, Functor, Traversable)

data Null a = Null
data Boolean a = Boolean
data Int a = Int
data Long a = Long
data Float a = Float
data Double a = Double
data Bytes a = Bytes
data String a = Text
data Array a = Array { _arrayItem :: a }
data Map a = Map { _mapValues :: a }

data Record a = Record {
  _recordName :: Text,
  _recordNamespace :: Maybe Text,
  _recordAliases :: [Text],
  _recordDoc :: Maybe Text ,
  _recordFields :: [Field a]
}
data Enum a = Enum {
  _enumName :: Text,
  _enumNamespace :: Maybe Text,
  _enumAliases :: [Text],
  _enumDoc :: Maybe Text,
  _enumSymbols :: [Text]
  }
data Union a = Union {
  options :: NonEmpty a
  }
data Fixed a = Fixed {
  name :: Text,
  namespace :: Maybe a,
  aliases :: [Text],
  size :: Integer
}
  deriving (Functor, Foldable, Traversable)
