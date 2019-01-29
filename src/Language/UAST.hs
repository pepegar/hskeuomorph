{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Language.UAST (
  NullF,
  BooleanF,
  IntF,
  LongF,
  FloatF,
  DoubleF,
  BytesF,
  StringF,
  ArrayF,
  MapF,
  RecordF,
  EnumF,
  UnionF,
  FixedF,
  Field
) where

import Data.Text
import Data.List.NonEmpty
import Data.Comp
import Data.Maybe
import qualified Data.Int as I

data Field a = Field {
  _fieldName :: Text,
  _fieldTpe :: a
  } deriving (Functor, Foldable, Traversable, Show, Eq)

data NullF a = Null
  deriving (Functor, Foldable, Traversable, Show, Eq)

data BooleanF a = Boolean
  deriving (Functor, Foldable, Traversable, Show, Eq)

data IntF a = Int
  deriving (Functor, Foldable, Traversable, Show, Eq)

data LongF a = Long
  deriving (Functor, Foldable, Traversable, Show, Eq)

data FloatF a = Float
  deriving (Functor, Foldable, Traversable, Show, Eq)

data DoubleF a = Double
  deriving (Functor, Foldable, Traversable, Show, Eq)

data BytesF a = Bytes
  deriving (Functor, Foldable, Traversable, Show, Eq)

data StringF a = Text
  deriving (Functor, Foldable, Traversable, Show, Eq)

data ArrayF a = Array { _arrayItem :: a }
 deriving (Functor, Foldable, Traversable, Show, Eq)

data MapF a = Map { _mapValues :: a }
 deriving (Functor, Foldable, Traversable, Show, Eq)

data RecordF a = Record {
  _recordName :: Text,
  _recordNamespace :: Maybe Text,
  _recordAliases :: [Text],
  _recordDoc :: Maybe Text ,
  _recordFields :: [Field a]
} deriving (Functor, Foldable, Traversable, Show, Eq)

data EnumF a = Enum {
  _enumName :: Text,
  _enumNamespace :: Maybe Text,
  _enumAliases :: [Text],
  _enumDoc :: Maybe Text,
  _enumSymbols :: [Text]
} deriving (Functor, Foldable, Traversable, Show, Eq)

data UnionF a = Union {
  options :: NonEmpty a
} deriving (Functor, Foldable, Traversable, Show, Eq)

data FixedF a = Fixed {
  name :: Text,
  namespace :: Maybe a,
  aliases :: [Text],
  size :: I.Int
} deriving (Functor, Foldable, Traversable, Show, Eq)


