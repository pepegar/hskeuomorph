module Command (
  validate,
  lint,
  convert
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Functor.Foldable
import           Language.Avro

data Command = Lint { file :: String }
             | Validate { file :: String }
             | Convert { to :: String, from :: String }

handleCommand :: Command -> IO ()
handleCommand (Lint file)       = lint file
handleCommand (Validate file)   = validate file
handleCommand (Convert to from) = convert to from


validate :: String -> IO ()
validate file = do
  contents <- readFile file
  putStrLn $ show $ toAvro contents

  where
    toAvro :: String -> Maybe (Fix AvroF)
    toAvro c = decode $ BL.pack c


lint :: String -> IO ()
lint _ = putStrLn "TODO"

convert :: String -> String -> IO ()
convert _ _ = putStrLn "TODO"

