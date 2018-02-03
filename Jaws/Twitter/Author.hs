{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Jaws.Twitter.Author where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
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
