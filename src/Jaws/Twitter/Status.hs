{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Jaws.Twitter.Status where

import           Control.Lens
import qualified Data.ByteString.Char8  as S8
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Jaws.Twitter.Author
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

env :: String -> IO S8.ByteString
env = return . S8.pack

authTokens :: Author -> IO (OAuth, Credential)
authTokens x = do
  ck <- env . consumerKey $ x
  cs <- env . consumerSecret $ x
  at <- env . accessToken $ x
  as <- env . accessSecret $ x
  let oauth = twitterOAuth
            { oauthConsumerKey =  ck
            , oauthConsumerSecret = cs
            }
      cred  = Credential
            [ ("oauth_token", at)
            , ("oauth_token_secret", as)
            ]
  return (oauth, cred)

getTWInfo :: Author -> IO TWInfo
getTWInfo x = do
  (oa, cred) <- authTokens x
  return $ setCredential oa cred def

updateStatus :: (Author, String) -> IO ()
updateStatus (author, status) = do
  mgr    <- newManager tlsManagerSettings
  twInfo <- getTWInfo author
  res    <- call twInfo mgr $ update (T.pack status)
  T.putStrLn $ res ^. statusText

