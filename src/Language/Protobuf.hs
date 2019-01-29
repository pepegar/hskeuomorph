{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.Protobuf(
  Field(..),
  ProtoF(..)
  ) where

import           Data.Functor.Foldable (Fix)
import           Data.List.NonEmpty
import qualified Data.Text             as T
import           Text.Show.Deriving

data Option = Option {
  _optionName  :: T.Text,
  _optionValue :: T.Text
  } deriving (Eq, Show)

data Field a = Field {
  _fieldName :: T.Text,
  _fieldTpe  :: a
  } deriving (Functor, Foldable, Traversable)

$(deriveShow1 ''Field)

data ProtoF a =
    Double
  | Float
  | Int32
  | Int64
  | Uint32
  | Uint64
  | Sint32
  | Sint64
  | Fixed32
  | Fixed64
  | Sfixed32
  | Sfixed64
  | Bool
  | String
  | Bytes
  | Optional { value :: a }
  | Repeated { value :: a }
  | Map { keys :: a, values :: a }
  | OneOf { ts :: NonEmpty a }
  | Enum {
      name    :: T.Text,
      symbols :: [(T.Text, Int)],
      options :: [Option],
      aliases :: [(T.Text, Int)]
      }
  | Message {
      name     :: T.Text,
      fields   :: [Field a],
      reserved :: [[T.Text]]
      }
  deriving (Functor, Foldable, Traversable)

$(deriveShow1 ''ProtoF)
