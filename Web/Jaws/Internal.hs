module Web.Jaws.Internal
    ( run
    ) where

import           Control.Lens               hiding (mapping)
import qualified Data.ByteString.Lazy.Char8 as Char8
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

run :: String -> String -> IO String
run mtd loc = do
  xs <- getData mtd loc
  return xs
