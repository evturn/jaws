module Main where

import           Jaws
import           System.Environment

main :: IO ()
main = do
  [args] <- getArgs
  printContents args
