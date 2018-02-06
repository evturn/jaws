module Jaws.Internal
    ( fetchSource
    ) where

import           Control.Lens               hiding (mapping)
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Network.Wreq

fetchSource :: String -> IO String
fetchSource url = do
  r  <- get url
  xs <- return $ r ^. responseBody
  return $ Char8.unpack xs
