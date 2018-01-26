{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Jaws.Twitter.Author where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T
import           GHC.Generics

data Author = Author
    { consumerKey    :: String
    , consumerSecret :: String
    , accessToken    :: String
    , accessSecret   :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

parseObj :: Value -> Parser Author
parseObj = withObject "object" $ \o ->
  Author <$> o .: "consumerKey"
         <*> o .: "consumerSecret"
         <*> o .: "accessToken"
         <*> o .: "accessSecret"

parseItUp :: IO (Parser Author)
parseItUp = do
  json <- BS.readFile "/Users/ev/src/dev/jaws/env.json"
  case decode json of
    Nothing -> return $ fail "shit fuck!"
    Just x  -> return $ parseObj (toJSON $ Object x)
