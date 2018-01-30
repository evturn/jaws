{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Jaws.Twitter.Status where

import           Control.Lens
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Jaws.Data
import           Jaws.Twitter.Author
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

getTWInfo :: Author -> IO TWInfo
getTWInfo x = do
  (oa, cred) <- authTokens x
  return $ setCredential oa cred def

updateStatus :: Int -> IO ()
updateStatus i = do
  x <- author i
  case x of
    Left _ -> putStrLn "shit."
    Right a -> do
      mgr <- newManager tlsManagerSettings
      twInfo <- getTWInfo a
      xs <- jaws "url" (contentURL a)
      putStrLn $ "\nPosting...\n"
      status <- call twInfo mgr $ update (T.pack xs)
      T.putStrLn $ status ^. statusText
