module Jaws.System.Random where

import           System.Random (Random, getStdRandom, randomR)

randomIntTo :: Int -> IO Int
randomIntTo x = getStdRandom (randomR (0, x))

randomIntFromTo :: Int -> Int -> IO Int
randomIntFromTo x y = getStdRandom (randomR (x, y))

randomSelect :: [String] -> IO String
randomSelect words = do
  index <- randomIntTo $ (length words) - 1
  return $ words !! index
