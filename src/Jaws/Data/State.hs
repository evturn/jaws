module Jaws.Data.State where

import           Control.Monad      (replicateM)
import           Data.Char          (toUpper)
import           Data.List          (unwords)
import           Jaws.Data.Mapping  (Map, keys, lookupSubmap, mapping,
                                     wordFrequencyList)
import           Jaws.System.Random (randomIntFromTo, randomSelect)


nextWord :: Map -> String -> IO String
nextWord mp x = randomSelect $ wordFrequencyList (lookupSubmap x mp)

build :: (String -> IO String) -> (String, String) -> IO (Maybe String)
build f (w, s) = do
  w' <- f w
  case w' of
    "" -> if (length $ words s) < 4
          then return $ Nothing
          else return $ Just s
    _  -> build f (w', s ++ " " ++ w')

initState :: [String] -> IO (String, String)
initState = (fmap getState . randomSelect)
  where
    caps xs = (toUpper <$> take 1 xs) ++ drop 1 xs
    getState = (,) <$> id <*> caps

execState :: [String] -> Map -> IO String
execState seeds mp = do
  s <- initState seeds
  maybeString <- build (nextWord mp) s
  case maybeString of
    Nothing -> execState seeds mp
    Just s' -> return s'

runRepeat :: Int -> Map -> IO String
runRepeat n mp = do
  unwords <$> replicateM n (execState (keys mp) mp)

runRepeatR :: Int -> Map -> IO String
runRepeatR n mp = do
  m <- randomIntFromTo 1 n
  runRepeat m mp

runJaws :: Map -> IO String
runJaws mp = do
  execState (keys mp) mp
