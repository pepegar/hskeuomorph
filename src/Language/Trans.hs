{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Language.Trans (
  transform
  ) where

import           Data.Functor.Foldable
import qualified Language.Avro         as AV
import qualified Language.Protobuf     as PB

convert :: AV.AvroF t -> PB.ProtoF t
convert (AV.Null)                                    = PB.Float -- TODO: WAT
convert (AV.Boolean)                                 = PB.Bool
convert (AV.Int)                                     = PB.Int32
convert (AV.Long)                                    = PB.Int64
convert (AV.Float)                                   = PB.Float
convert (AV.Double)                                  = PB.Float
convert (AV.Bytes)                                   = PB.Bytes
convert (AV.String)                                  = PB.String
convert (AV.Array item)                              = PB.Repeated item
convert (AV.Map values)                              = PB.Map values values --TODO: instantiate keys as string
convert (AV.Record name ns aliases doc order fields) = PB.Message name fs []
  where
    fs = avroFieldToProto <$> fields
    avroFieldToProto (AV.Field name tpe) = PB.Field name tpe
convert (AV.Enum name ns aliases doc symbols)        = PB.Enum name symbolsWithIndices [] [] -- TODO: i'm making indices up here, it's ugly
  where
    symbolsWithIndices = symbols `zip` [0..]
convert (AV.Union options)                           =
  PB.OneOf options
convert (AV.Fixed name ns aliases size)              = PB.Float -- TODO: WAT


transform :: Fix AV.AvroF -> Fix PB.ProtoF
transform = cata $ embed . convert
