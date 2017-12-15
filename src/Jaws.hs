module Jaws where

import           System.Environment (getArgs)

printContents :: IO ()
printContents = do
  [x] <- getArgs
  file <- readFile x
  putStrLn file
