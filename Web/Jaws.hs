{-# LANGUAGE OverloadedStrings #-}

module Web.Jaws (jaws) where

import           Web.Jaws.Internal (run)
import           Web.Jaws.State    (start)

jaws :: String -> String -> IO String
jaws mtd loc = do
  xs <- run mtd loc
  start xs
