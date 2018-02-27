module Data.Jaws.Debug where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Char                 (toUpper)
import           Data.Jaws.Mapping         (Map, keys, keysByFrequency,
                                            lookupSubmap, mapping)
import           Data.Jaws.State           (runRepeat, runRepeatR)
import           Data.Jaws.System.IO       (fetchSource)
import           Data.Jaws.System.Random   (randomSelect)
import           System.Environment        (getEnv)

-- | Set the URL env variable in the repl
--
-- `URL="https://<url>" stack ghci`
-- `import Jaws.Data.Debug`

type Seeds = [String]
type JawsState = String

jawsT :: String -> StateT String IO String
jawsT = return

selectFirst :: Seeds -> IO (String, JawsState)
selectFirst ss = do
  w <- randomSelect ss
  (runStateT $ withStateT caps (jawsT w)) w

selectNext :: Map -> String -> IO String
selectNext mp w = randomSelect $ keysByFrequency $ lookupSubmap w mp

isValid :: JawsState -> Maybe JawsState
isValid  s = case (length $ words s) >= 4 of
  True  -> Just s
  False -> Nothing

genSucc :: Map -> (String -> IO String)
genSucc m w = randomSelect $ keysByFrequency $ lookupSubmap w m

getInit :: Map -> IO (String, String)
getInit mp = do
  w <- randomSelect $ keys mp
  return (w, caps w)

runBuild :: (String -> IO String) -> (String, String) -> IO (String, String)
runBuild f (w, s) = do
  case w == "" of
    True -> return (w, s)
    False -> do
      w' <- f w
      runBuild f (w', s ++ " " ++ w')

updateState :: Map -> (String, String) -> IO (String, String)
updateState mp st = do
  runBuild (genSucc mp) st

-- Implementaion:
--
--       do
--         mp <- withMap
--         st <- getInit
--         updateState mp st
--
-- Then extract the final value
-- If validation doesn't check out retry until it passes.

build :: Map -> (String, JawsState) -> IO (Maybe JawsState)
build mp (w, s) = do
  w' <- selectNext mp w
  case w' of
    "" -> return $ isValid s
    _  -> build mp (w', s ++ " " ++ w')

build' :: (Seeds, Map) -> IO String
build' (ss, mp) = do
  (w, s) <- selectFirst ss
  maybeString <- build mp (w, s)
  case maybeString of
    Nothing -> build' (ss, mp)
    Just s' -> return s'

runJawsT :: IO JawsState
runJawsT = do
  (ss, mp) <- getJawsSource
  build' (ss, mp)

withMap :: IO Map
withMap = do
  loc <- getEnv "URL"
  mapping <$> fetchSource loc

getJawsSource :: IO (Seeds, Map)
getJawsSource = do
  mp <- withMap
  return (keys mp, mp)

caps :: String -> String
caps w = (toUpper <$> take 1 w) ++ drop 1 w

jaws :: Int -> String -> IO String
jaws n loc = do
  mp <- mapping <$> fetchSource loc
  runRepeat n (keys mp, mp)

jawsR :: Int -> String -> IO String
jawsR n loc = do
  mp <- mapping <$> fetchSource loc
  runRepeatR n (keys mp, mp)
