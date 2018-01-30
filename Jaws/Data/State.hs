module Jaws.Data.State where

import qualified Data.Char          as C
import           Data.Monoid
import           Jaws.System.Random

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

caps :: String -> String
caps xs = (C.toUpper . head) xs : tail xs

