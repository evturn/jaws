module Jaws.System.IO where

import           Control.Lens               hiding (mapping)
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Jaws.Data                  (mapping, runRepeatR)
import           Jaws.Twitter
import           Network.Wreq
import           System.Environment         (getArgs)

fetchSource :: String -> IO String
fetchSource url = do
  r  <- get url
  xs <- return $ r ^. responseBody
  return $ Char8.unpack xs

mkInt :: String -> Int
mkInt = read

execJaws :: IO ()
execJaws = do
  (config:index:count:[]) <- getArgs
  authors                 <- getJSON config
  case authors of
    Left e   -> putStrLn e
    Right as -> runUpdate (as !! (mkInt index)) (mkInt count)

runUpdate :: Author -> Int -> IO ()
runUpdate author count = do
  content <- fetchSource (contentURL author)
  status  <- runRepeatR count $ mapping content
  updateStatus (author, status)
