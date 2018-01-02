{-# LANGUAGE OverloadedStrings #-}

module Main
    ( fromFile
    , fromURL
    , jaws
    , main
    ) where

import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Jaws.Data
import           Jaws.Internal
import           Jaws.Text

getSelections :: Maybe Submap -> [String]
getSelections smp = foldr go [] $ M.toList (fromMaybe M.empty smp)
  where
    go (k, v) xs = xs ++ replicate v k

sumElems :: Maybe Submap -> Int
sumElems Nothing    = 0
sumElems (Just sps) = sum $ M.elems sps

seeds :: ReaderT Map [] String
seeds = ReaderT $ \r -> getSeeds r

getSeedValue :: [String] -> IO String
getSeedValue sds = do
  index <- getRandomInt (length $ sds)
  return $ sds !! index

getInitState :: Map -> IO (String, String)
getInitState mp = do
  sds <- return $ getSeeds mp
  seed <- getSeedValue sds
  return (seed, (caps seed))

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

run :: String -> IO ()
run xs = do
  mp <- return $ mapping xs
  st <- getInitState mp
  x  <- runBuilder st mp
  prettyPrint x

jaws :: String -> String -> IO ()
jaws mtd loc = do
  xs <- runJaws mtd loc
  run xs

main :: IO ()
main = do
  xs <- execJaws
  run xs
