module Jaws.State where

import           Jaws.Data     (Map, Submap, getSub, keys, mapping,
                                probabilities, subToList, subValues)
import           Jaws.Internal (getRandomInt, pick)
import           Jaws.Text     (caps)

getState :: String -> (String, String)
getState = (,) <$> id <*> caps

emptyS :: (String, String)
emptyS = (mempty, mempty)

sumSubValues :: Submap -> Int
sumSubValues sub = sum $ subValues sub

mergeState :: (String, String) -> (String, String) -> (String, String)
mergeState sta sta' = (fst sta', (snd sta) ++ " " ++ (snd sta'))

putState :: [String] -> (String, String) -> IO (String, String)
putState seeds sta = do
  seed <- pick seeds
  sta' <- return $ getState seed
  case sta of
    ("", "") -> return sta'
    _        -> return $ mergeState sta sta'

buildState :: [String] -> (String, String) -> Map -> IO String
buildState seeds sta mp = do
  sub        <- return $ getSub (fst sta) mp
  total      <- return $ sumSubValues sub
  selections <- return $ probabilities sub
  index      <- getRandomInt total
  word       <- return $ selections !! index
  case word of
    "" -> return $ snd sta
    _  -> buildState seeds (word, (snd sta ++ " " ++ word)) mp

start :: String -> IO ()
start path = do
  xs    <- readFile path
  mp    <- return $ mapping xs
  seeds <- return $ keys mp
  sta   <- putState seeds emptyS
  st    <- buildState seeds sta mp
  print st
