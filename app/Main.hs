{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Command               as C
import           Control.Monad         (when)
import           Data.Aeson
import           Language.Avro
import           System.Console.Docopt
import           System.Environment    (getArgs)

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "validate")) $ do
    file <- args `getArgOrExit` (argument "file")
    C.validate file

  when (args `isPresent` (command "convert")) $ do
    file <- args `getArgOrExit` (argument "file")
    from <- args `getArgOrExit` (argument "from")
    to <- args `getArgOrExit` (argument "to")
    C.convert file from to
