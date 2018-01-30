module Web.Jaws.State where

import           System.Random          (Random, getStdRandom, randomR)
import           Web.Jaws.Text          (caps)
import           Web.Jaws.Types.Mapping (Map, Submap, getSub, keys, mapping,
                                         probabilities, subToList, subValues)

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
  sub  <- return $ getSub (fst sta) mp
  word <- randomSelect $ probabilities sub
  case word of
    "" -> return $ snd sta
    _  -> buildState seeds (word, (snd sta ++ " " ++ word)) mp

start :: String -> IO String
start xs = do
  mp    <- return $ mapping xs
  seeds <- return $ keys mp
  sta   <- putState seeds emptyState
  buildState seeds sta mp

randomIntTo :: (Num a, Random a) => a -> IO a
randomIntTo x = getStdRandom (randomR (0, x - 1))

randomSelect :: [String] -> IO String
randomSelect words = do
  index <- randomIntTo (length words)
  return $ words !! index
