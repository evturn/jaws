{-# LANGUAGE OverloadedStrings #-}

module Jaws
    ( module Jaws.Data
    , module Jaws.System
    , module Jaws.Twitter
    , jaws
    , jawsR
    , once
    ) where

import           Jaws.Data
import           Jaws.System
import           Jaws.Twitter

once :: String -> IO String
once loc = do
  xs <- fetchSource loc
  runJaws $ mapping xs

jaws :: Int -> String -> IO String
jaws n loc = do
  xs <- fetchSource loc
  runRepeat n $ mapping xs

jawsR :: Int -> String -> IO String
jawsR n loc = do
  xs <- fetchSource loc
  runRepeatR n $ mapping xs
