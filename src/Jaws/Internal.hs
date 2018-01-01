module Jaws.Internal where

import           System.Random

getRandomInt :: (Num a, Random a) => a -> IO a
getRandomInt x = getStdRandom (randomR (0, x - 1))
