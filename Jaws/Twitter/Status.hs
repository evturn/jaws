{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Jaws.Twitter.Status where

import           Control.Lens
import qualified Data.ByteString.Char8  as S8
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Jaws.Data
import           Jaws.Twitter.Author
import           System.Environment
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

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

env :: String -> IO S8.ByteString
env = (S8.pack <$>) . getEnv

getTWInfo :: Author -> IO TWInfo
getTWInfo x = do
  (oa, cred) <- authTokens x
  return $ setCredential oa cred def

-- recur :: Int -> Int -> IO ()
-- recur index count = do
--   ea <- eitherAuthor 0
--   case ea of
--     Left e -> putStrLn e
--     Right author -> do
--       content <- getContent (contentURL author)
--       repeatRun content 3

updateWithAuthor :: Int -> IO ()
updateWithAuthor index = do
  authors <- getJSON
  case authors of
    Left _   -> putStrLn "well, damn."
    Right as -> runUpdate (as !! index)

runUpdate :: Author -> IO ()
runUpdate x = do
  (a, s) <- getAuthorWithStatus x
  updateStatus (a, s)

getAuthorWithStatus :: Author -> IO (Author, String)
getAuthorWithStatus author = do
  status  <- jaws "url" (contentURL author)
  return (author, status)

updateStatus :: (Author, String) -> IO ()
updateStatus (author, status) = do
  mgr    <- newManager tlsManagerSettings
  twInfo <- getTWInfo author
  res    <- call twInfo mgr $ update (T.pack status)
  T.putStrLn $ res ^. statusText
