{-# LANGUAGE OverloadedStrings #-}

module Web.Jaws
    ( jaws
    , main
    ) where

import           Web.Jaws.Internal (exec, run)
import           Web.Jaws.State    (start)

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
