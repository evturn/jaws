{-# LANGUAGE OverloadedStrings #-}

module Jaws.System where

import           Control.Lens               hiding (mapping)
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Monoid
import           Jaws.System.Random
import           Network.Wreq
import           System.Environment

fromURL :: String -> IO String
fromURL url = do
  r  <- get url
  xs <- return $ r ^. responseBody
  return $ Char8.unpack xs

getData :: String -> String -> IO String
getData  "file" loc = readFile loc
getData  _      loc = fromURL loc

