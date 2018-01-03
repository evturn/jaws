{-# LANGUAGE OverloadedStrings #-}

module Main
    ( fromFile
    , fromURL
    , jaws
    , main
    ) where

import qualified Data.Map      as M
import           Jaws.Data
import           Jaws.Internal
import           Jaws.Text

replicateByProbability :: (String, Int) -> [String] -> [String]
replicateByProbability (k, v) xs = xs ++ replicate v k

getSelections :: Maybe Submap -> [String]
getSelections sp = foldr replicateByProbability [] $ toList sp

sumElems :: Maybe Submap -> Int
sumElems Nothing    = 0
sumElems (Just sps) = sum $ M.elems sps

runBuilder :: (String, String) -> Map -> IO String
runBuilder state mp = do
  smp     <- return $ M.lookup (fst state) mp
  num     <- return $ sumElems smp
  choices <- return $ getSelections smp
  index   <- getRandomInt num
  word    <- return $ choices !! index
  case word of
    "" -> return $ snd state
    _  -> runBuilder (word, (snd state ++ " " ++ word)) mp

runJaws :: String -> IO ()
runJaws xs = do
  mp <- return $ mapping xs
  st <- getState mp
  x  <- runBuilder st mp
  prettyPrint x

jaws :: String -> String -> IO ()
jaws mtd loc = do
  xs <- run mtd loc
  runJaws xs

main :: IO ()
main = do
  xs <- exec
  runJaws xs

getState :: Map -> IO (String, String)
getState mp = do
  sds  <- return $ keys mp
  seed <- pick sds
  return (seed, (caps seed))

