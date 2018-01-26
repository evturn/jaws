{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Jaws.Twitter.Author where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as B
import           System.Environment
import           Web.Twitter.Conduit

data Author = Author
    { consumerKey    :: String
    , consumerSecret :: String
    , accessToken    :: String
    , accessSecret   :: String
    } deriving Show

instance FromJSON Author where
  parseJSON (Object x) = Author
    <$> x .: "consumerKey"
    <*> x .: "consumerSecret"
    <*> x .: "accessToken"
    <*> x .: "accessSecret"

instance ToJSON Author where
  toJSON (Author ck cs at as) = object
    [ "consumerKey"    .= ck
    , "consumerSecret" .= cs
    , "accessToken"    .= at
    , "accessSecret"   .= as
    ]

getJSON :: IO B.ByteString
getJSON = B.readFile "/Users/ev/src/dev/jaws/env.json"

maybeAuthor :: IO (Maybe Author)
maybeAuthor = decode <$> getJSON

parseItUp :: IO ()
parseItUp = do
  d <- maybeAuthor
  case d of
    Nothing -> return ()
    Just x  -> do
      val <- authTokens x
      print val

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
