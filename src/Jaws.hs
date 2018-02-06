{-# LANGUAGE OverloadedStrings #-}

module Jaws
    ( module Jaws.Data
    , module Jaws.System
    , module Jaws.Twitter
    , jaws
    , repeatRun
    ) where

import           Jaws.Data
import           Jaws.Internal (fetchSource)
import           Jaws.System
import           Jaws.Twitter

jaws :: String -> IO String
jaws loc = do
  xs <- fetchSource loc
  start xs

repeatRun :: String -> Int -> IO ()
repeatRun xs n = do
  res <- mapM start (replicate n xs)
  print $ foldr (\a b -> b ++ a ++ " ") "" res

