{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Jaws.Twitter.Status where

import           Control.Lens
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Web.Jaws.Twitter.Authentication
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

getTWInfo :: IO TWInfo
getTWInfo = do
  (oa, cred) <- getOAuthTokens
  return $ setCredential oa cred def

updateStatus :: IO ()
updateStatus = do
  mgr <- newManager tlsManagerSettings
  twInfo <- getTWInfo
  putStrLn $ "Posting..."
  status <- call twInfo mgr $ update (T.pack "800k likes and I'll eat breakfast.")
  T.putStrLn $ status ^. statusText
