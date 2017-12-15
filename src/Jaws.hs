module Jaws where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Char                 (isAlphaNum, toLower)
import           Data.List                 (lines, words)
import           System.Environment        (getArgs)
import           Text.Show.Pretty          (ppShow)

type WordMapping = (String, String, Int)

cleanChar :: Char -> Maybe Char
cleanChar x
    | isAlphaNum x = Just $ toLower x
    | otherwise    = Nothing

cleanChars :: String -> String
cleanChars xs = foldr go [] $ (fmap cleanChar xs)
  where
    go (Just a) b = a : b
    go Nothing b  = b

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

