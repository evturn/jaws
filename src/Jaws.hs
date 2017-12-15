module Jaws where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.List                 (lines)
import           System.Environment        (getArgs)


printContents :: FilePath -> IO ()
printContents p = do
  x <- readFile p
  y <- return $ lines x
  print y

