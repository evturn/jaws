module Jaws.State where

import           Data.IORef
import           Jaws.Data     (Map, keys, mapping)
import           Jaws.Internal (pick)
import           Jaws.Text     (caps)

makeMap :: FilePath -> IO (IORef Map)
makeMap fp = do
  xs <- readFile fp
  newIORef $ mapping xs

makeState :: String -> (String, String)
makeState = (,) <$> id <*> caps

getBuilderState :: IORef Map -> IO (String, String)
getBuilderState mapRef = do
  mp <- readIORef mapRef
  seed  <- pick $ keys mp
  return $ makeState seed

start :: String -> IO ()
start path = do
  mapRef <- makeMap path
  st <- getBuilderState mapRef
  print st
