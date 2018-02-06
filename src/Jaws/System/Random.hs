module Jaws.System.Random where

import           System.Random (Random, getStdRandom, randomR)

randomIntTo :: (Num a, Random a) => a -> IO a
randomIntTo x = getStdRandom (randomR (0, x))

randomSelect :: [String] -> IO String
randomSelect words = do
  index <- randomIntTo $ (length words) - 1
  return $ words !! index
