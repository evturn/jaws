module Jaws where

import           Data.Char        (isAlphaNum, toLower)
import           Data.List        (lines, words)
import qualified Data.Map         as M
import           Text.Show.Pretty (ppShow)

type Mapping = M.Map String (String, Int)

cleanChar :: Char -> Maybe Char
cleanChar x
    | isAlphaNum x = Just $ toLower x
    | otherwise    = Nothing

clean :: String -> String
clean xs = foldr go [] $ (fmap cleanChar xs)
  where
    go (Just a) b = a : b
    go Nothing b  = b

insert' :: String -> String -> Int -> Mapping -> Mapping
insert' x y n mp = M.insert (clean x) (clean y, n) mp

inc :: (String, Int) -> (String, Int)
inc (x, n) = (x, n + 1)

insert :: String -> String -> Mapping -> Mapping
insert x y mp = case M.lookup (clean x) mp of
                  Nothing      -> insert' x y 1 mp
                  Just (x', n) -> insert' x y (n + 1) mp

mapWords :: [[String]] -> Mapping
mapWords xs = foldr go M.empty xs
  where
    go (y:ys:yss) mp = go (ys:yss) (insert y ys mp)
    go _ mp          = mp

printContents :: FilePath -> IO ()
printContents p = do
  x <- readFile p
  xs <- return $ mapWords $ (fmap words) $ lines x
  putStrLn $ ppShow xs
