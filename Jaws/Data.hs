{-# LANGUAGE OverloadedStrings #-}

module Jaws.Data where

import           Jaws.Data.Mapping
import           Jaws.Data.State
import           Jaws.System

jaws :: String -> String -> IO String
jaws mtd loc = do
  xs <- run mtd loc
  start xs

run :: String -> String -> IO String
run mtd loc = do
  xs <- getData mtd loc
  return xs

start :: String -> IO String
start xs = do
  mp    <- return $ mapping xs
  seeds <- return $ keys mp
  sta   <- putState seeds emptyState
  buildState seeds sta mp
