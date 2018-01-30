{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Jaws.Twitter.Author where

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
    , contentURL     :: String
    } deriving Show

instance FromJSON Author where
  parseJSON (Object x) = Author
    <$> x .: "consumerKey"
    <*> x .: "consumerSecret"
    <*> x .: "accessToken"
    <*> x .: "accessSecret"
    <*> x .: "contentURL"

instance ToJSON Author where
  toJSON (Author ck cs at as cu) = object
    [ "consumerKey"    .= ck
    , "consumerSecret" .= cs
    , "accessToken"    .= at
    , "accessSecret"   .= as
    , "contentURL"     .= cu
    ]

readConfig :: IO B.ByteString
readConfig = do
  B.readFile "./env.json"

getJSON :: IO (Either String [Author])
getJSON = eitherDecode <$> readConfig

author :: Int -> IO (Either String Author)
author i = do
  e <- getJSON
  case e of
    Left _   -> return $ Left "well, damn."
    Right xs -> return $ Right (xs !! i)

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
