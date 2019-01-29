{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Print () where

import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Functor.Foldable
import           Data.Text
import           Data.Void
import qualified Language.Protobuf                    as PB


-- target language types modelled as phantom types
data Haskell
data Protobuf
data Avro


data Print a = Print { run :: a -> Text }

instance Contravariant Print where
  contramap f (Print p) = Print $ p . f

instance Divisible Print where
  divide f (Print fb) (Print fc) =
    Print $ \a ->
      case f a of
        (b, c) -> fb b <> fc c

  conquer = Print $ \x -> mempty

instance Decidable Print where
  choose f (Print fb) (Print fc) =
    Print $ \a ->
      case f a of
        Left x  -> fb x
        Right x -> fc x

  lose fx = Print $ \x -> absurd (fx x)


printProtoHaskell :: Print (Fix PB.ProtoF)
printProtoHaskell = Print $ cata algebra
  where
    algebra :: PB.ProtoF Text -> Text
    algebra PB.Double           = "double"
    algebra PB.Float            = "float"
    algebra PB.Int32            = "int32"
    algebra PB.Int64            = "unt64"
    algebra PB.Uint32           = "uint32"
    algebra PB.Uint64           = "uint64"
    algebra PB.Sint32           = "sint32"
    algebra PB.Sint64           = "sint64"
    algebra PB.Fixed32          = "fixed32"
    algebra PB.Fixed64          = "fixed64"
    algebra PB.Sfixed32         = "xfixed32"
    algebra PB.Sfixed64         = "sfixed64"
    algebra PB.Bool             = "bool"
    algebra PB.String           = "string"
    algebra PB.Bytes            = "bytes"
    algebra (PB.Optional value) = "optional " <> value
    algebra (PB.Repeated value) = "repeated " <> value
    algebra (PB.Map key value)  = "map<" <> key <> ", " <> value <> ">"
    algebra (PB.OneOf ts)       = "oneof {"

    -- AND RIGHT HERE I NOTICE THAT I NEED SOME SLEEP

class Printer f a where
  print :: Print (Fix f)

-- instance Printer PB.ProtoF Haskell where
--   print = _

