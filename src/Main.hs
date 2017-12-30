{-# LANGUAGE OverloadedStrings #-}

module Main
    ( fromFile
    , fromURL
    , jaws
    , main
    ) where

import           Control.Lens               hiding (mapping)
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Jaws.Data
import           Jaws.Text
import           Network.Wreq
import           System.Environment
import           System.Random

getRandomInt :: (Num a, Random a) => a -> IO a
getRandomInt x = getStdRandom (randomR (0, x - 1))

getSelections :: Maybe Submap -> [String]
getSelections smp = foldr go [] $ M.toList (fromMaybe M.empty smp)
  where
    go (k, v) xs = xs ++ replicate v k

sumElems :: Maybe Submap -> Int
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

fromFile :: FilePath -> IO String
fromFile p = do
  readFile p

fromURL :: String -> IO String
fromURL url = do
  r  <- get url
  xs <- return $ r ^. responseBody
  return $ Char8.unpack xs

getSourceString :: [String] -> IO String
getSourceString  ("file":loc:[]) = fromFile loc
getSourceString  (src:loc:[])    = fromURL loc

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

runJaws :: Map -> IO String
runJaws mp = do
  state <- getInitState mp
  runBuilder state mp

jaws :: IO ()
jaws = do
  args   <- getArgs
  xs     <- getSourceString args
  mp     <- return $ mapping xs
  result <- runJaws mp
  prettyPrint result

main :: IO ()
main = do jaws
