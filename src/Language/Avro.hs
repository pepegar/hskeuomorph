{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Language.Avro (
  Field(..),
  Order(..),
  AvroF(..)
  ) where

import Control.Category hiding ((.))
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Data.Functor.Foldable (Fix, cata)
import Data.Functor.Foldable.Exotic (anaM)
import Data.Functor.Classes (Show1)
import           Data.Aeson (FromJSON (..), ToJSON (..), object, (.!=), (.:), (.:!), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import Data.Scientific as S hiding (Fixed)
import Text.Show.Deriving

data Field a = Field {
  _fieldName :: T.Text,
  _fieldTpe :: a
  } deriving (Functor, Foldable, Traversable)

$(deriveShow1 ''Field)

instance J.FromJSON (Field J.Value) where
  parseJSON (J.Object o) = Field <$> o .: "name"
                                 <*> o .: "type"

data Order = Ascending
           | Descending
           | Ignore
           deriving (Show, Eq)

instance J.FromJSON Order where
  parseJSON (J.String "ascending") = return Ascending
  parseJSON (J.String "descending") = return Descending
  parseJSON (J.String "ignore") = return Ignore
  parseJSON _ = fail "expected [ascending|descending|ignore]"

data AvroF a = Null
             | Boolean
             | Int
             | Long
             | Float
             | Double
             | Bytes
             | String
             | Array { _arrayItem :: a }
             | Map { _mapValues :: a }
             | Record {
               _recordName :: T.Text,
               _recordNamespace :: Maybe T.Text,
               _recordAliases :: [T.Text],
               _recordDoc :: Maybe T.Text ,
               _recordOrder :: Maybe Order ,               
               _recordFields :: [Field a]
             }
             | Enum {
               _enumName :: T.Text,
               _enumNamespace :: Maybe T.Text,
               _enumAliases :: [T.Text],
               _enumDoc :: Maybe T.Text,
               _enumSymbols :: [T.Text]
             }
             | Union {
               options :: NE.NonEmpty a
             }
             | Fixed {
               name :: T.Text,
               namespace :: Maybe a,
               aliases :: [T.Text],
               size :: Int
             }
             deriving (Functor, Foldable, Traversable)

$(deriveShow1 ''AvroF)
  

printerAlg :: AvroF J.Value -> J.Value
printerAlg Null = J.object [("type", "null")]
printerAlg Boolean = J.object [("type", "boolean")]
printerAlg Int = J.object [("type", "int")]
printerAlg Long = J.object [("type", "long")]
printerAlg Float = J.object [("type", "float")]
printerAlg Double = J.object [("type", "double")]
printerAlg Bytes = J.object [("type", "bytes")]
printerAlg String = J.object [("type", "string")]
printerAlg (Array item) = J.object [("type", "array"), ("items", item)]
printerAlg (Map values) =  J.object [("type", "array"), ("values", values)]
printerAlg (Record name ns aliases doc order fields) =
  J.object [
    ("type", J.String "record"),
    ("name", J.String name),
    ("fields", J.Array $ fieldToJson <$> V.fromList fields)
  ]
  where
    fieldToJson :: Field J.Value -> J.Value
    fieldToJson (Field name tpe) = J.object[
      ("name", J.String name),
      ("type", tpe)
      ]
-- {"type": "enum", "name": "Foo", "symbols": ["A", "B", "C", "D"] }
printerAlg (Enum name ns aliases doc symbols) =
  J.object [
    ("type", J.String "enum"),
    ("name", J.String name),
    ("symbols", J.Array $ J.String <$> V.fromList symbols)
  ]
printerAlg (Union options) = J.Array <<< V.fromList <<< NE.toList $ options
-- {"type": "fixed", "size": 16, "name": "md5"}
printerAlg (Fixed name ns aliases size) =
  J.object [
    ("type", J.String "fixed"),
    ("name", J.String "name"),
    ("size", J.Number $ S.unsafeFromRational 3)
  ]

instance J.ToJSON (Fix AvroF) where
  toJSON = cata printerAlg

parseAvroFJSON :: J.Value -> J.Parser (AvroF J.Value)
parseAvroFJSON x = case x of
  J.String s -> case s of
    "null"    -> return $ Null
    "boolean" -> return $ Boolean
    "int"     -> return $ Int
    "long"    -> return $ Long
    "float"   -> return $ Float
    "double"  -> return $ Double
    "bytes"   -> return $ Bytes
    "string"  -> return $ String
    somename  -> fail $ "I don't understand type " <> T.unpack somename
  J.Array x | V.length x > 0 -> return $ Union $ NE.fromList $ V.toList x
            | otherwise -> fail $ "unions should at least have one element"
  J.Object obj -> do
    logicalType :: Maybe T.Text <- obj .:? "logicalType"
    tpe         :: T.Text <- obj .: "type"

    case tpe of
      "map" -> Map <$> obj .: "values" :: J.Parser (AvroF J.Value)
      "array" -> Map <$> obj .: "items" :: J.Parser (AvroF J.Value)
      "record" -> Record <$> obj .: "name"
                         <*> obj .:? "namespace"
                         <*> obj .:? "aliases" .!= []
                         <*> obj .:? "doc"
                         <*> obj .:? "order" .!= Just Ascending
                         <*> obj .: "fields"
      "enum" -> Enum <$> obj .: "name"
                     <*> obj .:? "namespace"
                     <*> obj .:? "aliases" .!= []
                     <*> obj .:? "doc"
                     <*> obj .: "symbols"
      "fixed" -> Fixed <$> obj .: "name"
                       <*> obj .:? "namespace"
                       <*> obj .:? "aliases" .!= []
                       <*> obj .: "size"
    
instance J.FromJSON (Fix AvroF) where
  parseJSON = anaM parseAvroFJSON
