{-# LANGUAGE OverloadedStrings #-}

module Data.Jaws.Twitter.Author where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as B

data Author = Author
    { consumerKey    :: String
    , consumerSecret :: String
    , accessToken    :: String
    , accessSecret   :: String
    , contentURL     :: String
    , index          :: Int
    } deriving Show

instance FromJSON Author where
  parseJSON (Object x) = Author
    <$> x .: "consumerKey"
    <*> x .: "consumerSecret"
    <*> x .: "accessToken"
    <*> x .: "accessSecret"
    <*> x .: "contentURL"
    <*> x .: "index"

instance ToJSON Author where
  toJSON (Author ck cs at as cu ix) = object
    [ "consumerKey"    .= ck
    , "consumerSecret" .= cs
    , "accessToken"    .= at
    , "accessSecret"   .= as
    , "contentURL"     .= cu
    , "index"          .= ix
    ]

getJSON :: String -> IO (Either String [Author])
getJSON filepath = eitherDecode <$> B.readFile filepath
