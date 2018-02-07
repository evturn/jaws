module Jaws.Data.State where

import           Data.Char          (toUpper)
import           Jaws.Data.Mapping  (Map, keys, lookupSubmap, mapping,
                                     wordFrequencyList)
import           Jaws.System.Random (randomSelect)

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

runJaws :: String -> IO String
runJaws xs = do
  mp    <- return $ mapping xs
  seeds <- return $ keys mp
  execState seeds mp
