{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Command as C
import Control.Monad (when)
import Language.Avro
import Data.Aeson
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "validate")) $ do
    file <- args `getArgOrExit` (argument "file")
    C.validate file
