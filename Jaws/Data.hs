{-# LANGUAGE OverloadedStrings #-}

module Jaws.Data where

import           Jaws.Data.Mapping
import           Jaws.Data.State
import           Jaws.System

jaws :: String -> String -> IO String
jaws mtd loc = do
  xs <- runJaws mtd loc
  start xs

runJaws :: String -> String -> IO String
runJaws mtd loc = do
  xs <- getData mtd loc
  return xs

start :: String -> IO String
start xs = do
  mp    <- return $ mapping xs
  seeds <- return $ keys mp
  sta   <- putState seeds emptyState
  buildState seeds sta mp

repeatRun :: String -> Int -> IO ()
repeatRun xs n = do
  res <- mapM start (replicate n xs)
  print $ foldr (\a b -> b ++ a ++ " ") "" res

getContent :: String -> IO String
getContent src = do
  getData "url" src
