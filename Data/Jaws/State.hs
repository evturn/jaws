module Data.Jaws.State where

import           Control.Monad             (replicateM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Char                 (toUpper)
import           Data.Jaws.Mapping         (Map, keys, keysByFrequency,
                                            lookupSubmap)
import           Data.Jaws.System.Random   (randomIntFromTo, randomSelect)
import           Data.List                 (unwords)

type Seeds     = [String]
type JawsState = String

jawsT :: String -> StateT String IO String
jawsT = return

caps :: String -> String
caps w = (toUpper <$> take 1 w) ++ drop 1 w

selectFirst :: Seeds -> IO (String, JawsState)
selectFirst ss = do
  w <- randomSelect ss
  (runStateT $ withStateT caps (jawsT w)) w

selectNext :: Map -> String -> IO String
selectNext mp w = randomSelect $ keysByFrequency $ lookupSubmap w mp

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

isValid :: JawsState -> Maybe JawsState
isValid s = case (length $ words s) >= 4 of
  True  -> Just s
  False -> Nothing

runRepeat :: Int -> (Seeds, Map) -> IO String
runRepeat n (ss, mp) = do
  unwords <$> replicateM n (build' (ss, mp))

runRepeatR :: Int -> (Seeds, Map) -> IO String
runRepeatR n (ss, mp) = do
  n' <- randomIntFromTo 1 n
  runRepeat n' (ss, mp)
