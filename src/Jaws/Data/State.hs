module Jaws.Data.State where

import           Control.Monad             (replicateM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Char                 (toUpper)
import           Data.List                 (unwords)
import           Jaws.Data.Mapping         (Map, keys, lookupSubmap,
                                            wordFrequencyList)
import           Jaws.System.Random        (randomIntFromTo, randomSelect)

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
selectNext mp w = randomSelect $ wordFrequencyList $ lookupSubmap w mp

build :: Map -> (String, JawsState) -> IO (Maybe JawsState)
build mp (w, s) = do
  w' <- selectNext mp w
  case w' of
    "" -> return $ validateFinalState s
    _  -> build mp (w', s ++ " " ++ w')

build' :: (Seeds, Map) -> IO String
build' (ss, mp) = do
  (w, s) <- selectFirst ss
  maybeString <- build mp (w, s)
  case maybeString of
    Nothing -> build' (ss, mp)
    Just s' -> return s'

validateFinalState :: JawsState -> Maybe JawsState
validateFinalState s = case (length $ words s) >= 4 of
  True  -> Just s
  False -> Nothing

runRepeat :: Int -> (Seeds, Map) -> IO String
runRepeat n (ss, mp) = do
  unwords <$> replicateM n (build' (ss, mp))

runRepeatR :: Int -> (Seeds, Map) -> IO String
runRepeatR n (ss, mp) = do
  n' <- randomIntFromTo 1 n
  runRepeat n' (ss, mp)
