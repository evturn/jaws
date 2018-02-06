{-# LANGUAGE OverloadedStrings #-}

module Jaws.Data
  ( module Jaws.Data.Mapping
  , module Jaws.Data.State
  , start
  ) where

import           Jaws.Data.Mapping
import           Jaws.Data.State

start :: String -> IO String
start xs = do
  mp    <- return $ mapping xs
  seeds <- return $ keys mp
  sta   <- putState seeds emptyState
  buildState seeds sta mp
