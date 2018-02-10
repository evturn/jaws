{-# LANGUAGE OverloadedStrings #-}

module Jaws
    ( module Jaws.Data
    , module Jaws.System
    , module Jaws.Twitter
    , jaws
    , jaws'
    ) where

import           Jaws.Data
import           Jaws.System
import           Jaws.Twitter

jaws :: String -> IO String
jaws loc = do
  xs <- fetchSource loc
  runJaws $ mapping xs

jaws' :: Int -> String -> IO String
jaws' n loc = do
  xs <- fetchSource loc
  runRepeatR n $ mapping xs
