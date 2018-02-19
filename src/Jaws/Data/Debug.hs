module Jaws.Data.Debug where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Char                 (toUpper)
import           Jaws.Data.Mapping         (Map, keys, mapping)
import           Jaws.System.IO            (fetchSource)
import           Jaws.System.Random        (randomSelect)
import           System.Environment        (getEnv)

--
-- | Set URL env variable on stack repl startup:
-- `URL="https://<url>" stack ghci`

type Seeds = [String]

withMap :: IO Map
withMap = do
  loc <- getEnv "URL"
  mapping <$> fetchSource loc

getJawsSource :: IO (Seeds, Map)
getJawsSource = do
  m <- withMap
  return (keys m, m)

jawsT :: String -> StateT String (MaybeT IO) String
jawsT = return

caps :: String -> String
caps xs = (toUpper <$> take 1 xs) ++ drop 1 xs

selectFirstWord :: Seeds -> IO (Maybe (String, String))
selectFirstWord seeds = do
  word <- randomSelect seeds
  runMaybeT $ runStateT (jawsT word) (caps word)

runJawsT :: IO (Maybe (String, String))
runJawsT = do
  (s, m) <- getJawsSource
  selectFirstWord s
