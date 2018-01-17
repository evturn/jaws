{-# LANGUAGE OverloadedStrings #-}

module Main
    ( jaws
    , main
    ) where

import           Jaws.Internal (exec, run)
import           Jaws.State    (start)

jaws :: String -> String -> IO ()
jaws mtd loc = do
  xs <- run mtd loc
  ys <- start xs
  putStrLn ys

main :: IO ()
main = do
  xs <- exec
  ys <- start xs
  putStrLn ys
