module Jaws.Data.State where

import           Data.Char          (toUpper)
import           Data.Monoid
import           Jaws.Data.Mapping  (Map, keys, lookupSubmap, mapping,
                                     wordFrequencyList)
import           Jaws.System.Random (randomSelect)

injectState :: String -> (String, String)
injectState = (,) <$> id <*> caps

emptyState :: (String, String)
emptyState = (mempty, mempty)

initState :: [String] -> (String, String) -> IO (String, String)
initState seeds sta = do
  sta' <- fmap injectState $ randomSelect seeds
  return $ case sta of
    ("", "") -> sta'
    _        -> (fst sta', (snd sta) ++ " " ++ (snd sta'))

buildState :: [String] -> (String, String) -> Map -> IO String
buildState seeds sta mp = do
  sub           <- return $ lookupSubmap (fst sta) mp
  wordSelection <- randomSelect $ wordFrequencyList sub
  case wordSelection of
    "" -> return $ snd sta
    _  -> buildState seeds (wordSelection, (snd sta ++ " " ++ wordSelection)) mp

runJaws :: String -> IO String
runJaws xs = do
  mp    <- return $ mapping xs
  seeds <- return $ keys mp
  sta   <- initState seeds emptyState
  buildState seeds sta mp

caps :: String -> String
caps xs = (toUpper . head) xs : tail xs

