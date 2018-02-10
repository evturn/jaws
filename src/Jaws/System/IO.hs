module Jaws.System.IO where

import           Control.Lens               hiding (mapping)
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Jaws.Data                  (mapping, runJaws)
import           Jaws.Twitter
import           Network.Wreq
import           System.Environment         (getArgs)

fetchSource :: String -> IO String
fetchSource url = do
  r  <- get url
  xs <- return $ r ^. responseBody
  return $ Char8.unpack xs

execJaws :: IO ()
execJaws = do
  (config:index:[]) <- getArgs
  authors           <- getJSON config
  case authors of
    Left e   -> putStrLn e
    Right as -> runUpdate (as !! ((read index) :: Int))

runUpdate :: Author -> IO ()
runUpdate author = do
  content <- fetchSource (contentURL author)
  status  <- runJaws $ mapping content
  updateStatus (author, status)
