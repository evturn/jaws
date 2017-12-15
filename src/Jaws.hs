module Jaws where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Char                 (isAlphaNum, toLower)
import           Data.List                 (lines, words)
import           System.Environment        (getArgs)
import           Text.Show.Pretty          (ppShow)

type Actual  = String
type Cleaned = String
type Next    = String

data WordMapping = Mapping Cleaned (Actual, Next)
  deriving (Eq, Show)


cleanChar :: Char -> Maybe Char
cleanChar x
    | isAlphaNum x = Just $ toLower x
    | otherwise    = Nothing

cleanChars :: String -> Cleaned
cleanChars xs = foldr go [] $ (fmap cleanChar xs)
  where
    go (Just a) b = a : b
    go Nothing b  = b

runClean :: String -> String -> WordMapping
runClean xs ys = Mapping (cleanChars xs) (xs, cleanChars ys)

mapWords :: [String] -> [WordMapping]
mapWords = go
  where
    go (x:xs:xss) = runClean x xs : mapWords (xs:xss)
    go _          = []

printContents :: FilePath -> IO ()
printContents p = do
  x <- readFile p
  y <- return $ concat $ fmap (mapWords . words) $ lines x
  putStrLn $ ppShow y

