module Jaws.Internal where

import           Control.Lens               hiding (mapping)
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Network.Wreq
import           System.Environment
import           System.Random

getRandomInt :: (Num a, Random a) => a -> IO a
getRandomInt x = getStdRandom (randomR (0, x - 1))

fromFile :: FilePath -> IO String
fromFile p = do
  readFile p

fromURL :: String -> IO String
fromURL url = do
  r  <- get url
  xs <- return $ r ^. responseBody
  return $ Char8.unpack xs

getData :: String -> String -> IO String
getData  "file" loc = fromFile loc
getData  _      loc = fromURL loc

execJaws :: IO String
execJaws = do
  (mtd:loc:[]) <- getArgs
  runJaws mtd loc

runJaws :: String -> String -> IO String
runJaws mtd loc = do
  xs <- getData mtd loc
  return xs
