module Jaws where

import           Data.Char        (isAlphaNum, toLower)
import           Data.List        (lines, words)
import           Text.Show.Pretty (ppShow)

type Actual  = String
type Cleaned = String
type Next    = String

data WordMapping a = No
                   | Mapping a (WordMapping a)
                   deriving (Eq, Show)

cleanChar :: Char -> Maybe Char
cleanChar x
    | isAlphaNum x = Just $ toLower x
    | otherwise    = Nothing

cleanChars :: String -> String
cleanChars xs = foldr go [] $ (fmap cleanChar xs)
  where
    go (Just a) b = a : b
    go Nothing b  = b


mapWords :: [String] -> WordMapping String
mapWords = go
  where
    go (x:xs:xss) = Mapping (cleanChars x) (go (xs:xss))
    go _          = No

printContents :: FilePath -> IO ()
printContents p = do
  x <- readFile p
  y <- return $ fmap (mapWords . words) $ lines x
  putStrLn $ ppShow y

