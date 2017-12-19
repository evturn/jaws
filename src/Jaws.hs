{-# LANGUAGE OverloadedStrings #-}

module Jaws
    ( fromFile
    , fromURL
    , jaws
    ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Char                  (isAlphaNum, toLower, toUpper)
import           Data.List                  (lines, nub, words)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Network.Wreq
import           System.Environment
import           System.Random
import           Text.Show.Pretty           (pPrint)

type Mapping a = M.Map String a
type Map       = Mapping SubMap
type SubMap    = Mapping Int

insert :: String -> String -> Map -> Map
insert k1 k2 mp = case M.lookup k1 mp of
  Nothing -> M.insert k1 (M.singleton k2 1) mp
  Just sp -> M.insert k1 (insertSubMap (M.lookup k2 sp) k2 sp) mp

insertSubMap :: Maybe Int -> String -> SubMap -> SubMap
insertSubMap Nothing  k sp = M.insert k 1 sp
insertSubMap (Just n) k sp = M.insert k (n + 1) sp

caps :: String -> String
caps xs = (toUpper . head) xs : tail xs

getRandomInt :: (Num a, Random a) => a -> IO a
getRandomInt x = getStdRandom (randomR (0, x - 1))

parseFile :: String -> [[String]]
parseFile xs = (fmap words) $ lines xs

mapWords :: [[String]] -> Map
mapWords = foldr go M.empty
  where
    go (x:xs:xss) mp = go (xs:xss) (insert x xs mp)
    go (x:[])     mp = insert x "" mp
    go []         mp = mp

createMap :: String -> Map
createMap = (mapWords . parseFile)

getSelections :: Maybe SubMap -> [String]
getSelections smp = foldr go [] $ M.toList (fromMaybe M.empty smp)
  where
    go (k, v) xs = xs ++ replicate v k

sumElems :: Maybe SubMap -> Int
sumElems Nothing     = 0
sumElems (Just smps) = sum $ M.elems smps

getInitValues :: Map -> [String]
getInitValues mp = M.foldrWithKey go [] mp
  where
    go k smp ks = case M.notMember "" smp of
                    True  -> k : ks
                    False -> ks

getInitValue :: Map -> IO String
getInitValue mp = do
  seeds <- return $ getInitValues mp
  index <- getRandomInt (length $ seeds)
  return $ seeds !! index

getInitState :: Map -> IO (String, String)
getInitState mp = do
  seed <- getInitValue mp
  return (seed, (caps seed))

runBuilder :: (String, String) -> Map -> IO String
runBuilder state mp = do
  smp     <- return $ M.lookup (fst state) mp
  num     <- return $ sumElems smp
  choices <- return $ getSelections smp
  index   <- getRandomInt num
  word    <- return $ choices !! index
  case word of
    "" -> return $ snd state
    _  -> runBuilder (word, (snd state ++ " " ++ word)) mp

printContents :: String -> IO ()
printContents xs = do
  mp    <- return $ createMap xs
  state <- getInitState mp
  str   <- runBuilder state mp
  pPrint str

fromFile :: FilePath -> IO ()
fromFile p = do
  xs <- readFile p
  printContents xs

fromURL :: String -> IO ()
fromURL url = do
  r  <- get url
  xs <- return $ r ^. responseBody
  printContents (Char8.unpack xs)

getCommand :: [String] -> (String -> IO (), String)
getCommand ("file":loc:[]) = (fromFile, loc)
getCommand (src:loc:[])    = (fromURL, loc)

jaws :: IO ()
jaws = do
  xs <- getArgs
  (f, x) <- return $ getCommand xs
  f x
