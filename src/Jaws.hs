module Jaws where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.List                 (lines, words)
import           Data.Map
import           System.Environment        (getArgs)
import           Text.Show.Pretty          (ppShow)

type WordMapping = (String, String, Int)

mapWords :: [String] -> [WordMapping]
mapWords = go
  where
    go (x:xs:xss) = (x, xs, 1) : mapWords xss
    go _          = []

printContents :: FilePath -> IO ()
printContents p = do
  x <- readFile p
  y <- return $ concat $ fmap (mapWords . words) $ lines x
  putStrLn $ ppShow y

