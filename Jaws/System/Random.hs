module Jaws.System.Random where

import           System.Random (Random, getStdRandom, randomR)

randomIntTo :: (Num a, Random a) => a -> IO a
randomIntTo x = getStdRandom (randomR (0, x - 1))

randomSelect :: [String] -> IO String
randomSelect words = do
  index <- randomIntTo (length words)
  return $ words !! index
