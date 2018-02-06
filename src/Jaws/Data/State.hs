module Jaws.Data.State where

import           Data.Char          (toUpper)
import           Data.Monoid
import           Jaws.Data.Mapping  (Map, keys, lookupSubmap, mapping,
                                     wordFrequencyList)
import           Jaws.System.Random (randomSelect)

emptyState :: (String, String)
emptyState = (mempty, mempty)

getState :: String -> (String, String)
getState = (,) <$> id <*> caps

mergeState :: (String, String) -> (String, String) -> (String, String)
mergeState sta sta' = (fst sta', (snd sta) ++ " " ++ (snd sta'))

putState :: [String] -> (String, String) -> IO (String, String)
putState seeds sta = do
  seed <- randomSelect seeds
  sta' <- return $ getState seed
  case sta of
    ("", "") -> return sta'
    _        -> return $ mergeState sta sta'

buildState :: [String] -> (String, String) -> Map -> IO String
buildState seeds sta mp = do
  sub  <- return $ lookupSubmap (fst sta) mp
  word <- randomSelect $ wordFrequencyList sub
  case word of
    "" -> return $ snd sta
    _  -> buildState seeds (word, (snd sta ++ " " ++ word)) mp

runJaws :: String -> IO String
runJaws xs = do
  mp    <- return $ mapping xs
  seeds <- return $ keys mp
  sta   <- putState seeds emptyState
  buildState seeds sta mp

caps :: String -> String
caps xs = (toUpper . head) xs : tail xs

