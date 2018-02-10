module Jaws.Data.State where

import           Control.Monad      (replicateM)
import           Data.Char          (toUpper)
import           Data.List          (unwords)
import           Jaws.Data.Mapping  (Map, keys, lookupSubmap, mapping,
                                     wordFrequencyList)
import           Jaws.System.Random (randomIntFromTo, randomSelect)

initState :: [String] -> IO (String, String)
initState seeds = do
  injectState <$> randomSelect seeds

injectState :: String -> (String, String)
injectState = (,) <$> id <*> caps

caps :: String -> String
caps xs = (toUpper . head) xs : tail xs

nextWord :: Map -> String -> IO String
nextWord mp x = randomSelect $ wordFrequencyList (lookupSubmap x mp)

build :: (String -> IO String) -> (String, String) -> IO String
build f (x, s) = do
  y <- f x
  case y of
    "" -> return s
    _  -> build f (y, s ++ " " ++ y)

execState :: [String] -> Map -> IO String
execState seeds mp = do
  initState seeds >>= (build (nextWord mp))

runRepeat :: Int -> Map -> IO String
runRepeat n mp = do
  unwords <$> replicateM n (execState (keys mp) mp)

runRepeatR :: Int -> Map -> IO String
runRepeatR n mp = do
  m <- randomIntFromTo 1 n
  runRepeat m mp

runJaws :: Map -> IO String
runJaws mp = do
  execState (keys mp) mp
