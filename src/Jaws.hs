module Jaws where

import           Data.Char        (isAlphaNum, toLower)
import           Data.List        (lines, words)
import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe)
import           System.Random
import           Text.Show.Pretty (ppShow)

type SubMapping = M.Map String Int
type Mapping    = M.Map String SubMapping

-- TODO recursively execute this pattern:
--
-- build :: String -> String -> Mapping -> String
--
-- submap     <- return $ getSubmap str mp
-- number     <- return $ distOfSubmap submap
-- selections <- return $ getSelections submap
--
-- Later inside IO:
-- index      <- getRandomInt number
-- word       <- return $ choices !! index
--
-- Recursive call:
-- build sentence word mp

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

getSubmap :: String -> Mapping -> Maybe SubMapping
getSubmap str mp = M.lookup str mp

mapFromMaybe :: Maybe SubMapping -> SubMapping
mapFromMaybe = fromMaybe M.empty

getSelections :: Maybe SubMapping -> [String]
getSelections smp = foldr go [] $ M.toList (mapFromMaybe smp)
  where
    go (k, v) xs = xs ++ replicate v k

distOfSubmap :: Maybe SubMapping -> Int
distOfSubmap Nothing     = 0
distOfSubmap (Just smps) = sum $ M.elems smps

printContents :: FilePath -> IO ()
printContents p = do
  xs      <- readFile p
  mp      <- return $ mapWords $ (fmap words) $ lines xs
  smp     <- return $ getSubmap "started" mp
  num     <- return $ distOfSubmap smp
  choices <- return $ getSelections smp
  index   <- getRandomInt num
  word    <- return $ choices !! index
  putStrLn $ ppShow word
