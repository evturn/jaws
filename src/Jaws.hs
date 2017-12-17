module Jaws where

import           Data.Char        (isAlphaNum, toLower)
import           Data.List        (lines, words)
import qualified Data.Map         as M
import           System.Random
import           Text.Show.Pretty (ppShow)

type SubMapping = M.Map String Int
type Mapping    = M.Map String SubMapping

-- TODO: Randomize key extraction
extractKey :: SubMapping -> String
extractKey mp = head $ M.keys mp

build :: String -> String -> Mapping -> String
build xs k mp = case M.lookup k mp of
                  Nothing  -> xs ++ " " ++ k
                  Just smp -> build (xs ++ " " ++ k) (extractKey smp) mp

insert :: String -> String -> Mapping -> Mapping
insert x y mp = case M.lookup x mp of
  Nothing  -> M.insert x (M.singleton y 1) mp
  Just smp -> case M.lookup y smp of
                Nothing -> M.insert x (M.insert y 1 smp) mp
                Just n  -> M.insert x (M.insert y (n + 1) smp) mp

getRandomInt :: (Num a, Random a) => a -> IO a
getRandomInt x = getStdRandom (randomR (0, x - 1))

mapWords :: [[String]] -> Mapping
mapWords xs = foldr go M.empty xs
  where
    go (y:ys:yss) mp = go (ys:yss) (insert y ys mp)
    go (x:[])     mp = insert x "" mp
    go []         mp = mp

distOfSubmap :: String -> Mapping -> Int
distOfSubmap str mp = case M.lookup str mp of
  Nothing   -> 0
  Just smps -> sum $ M.elems smps

printContents :: FilePath -> IO ()
printContents p = do
  xs <- readFile p
  mp <- return $ mapWords $ (fmap words) $ lines xs
  re <- return $ distOfSubmap "to" mp
  putStrLn $ ppShow re
