{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Jaws.System.Cron where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as B
import           Data.Monoid
import qualified Data.Text             as T
import           Jaws.Data
import           Jaws.Twitter.Status
import           System.Cron
import           System.Environment
import           Web.Twitter.Conduit

data Cron = Cron
    { index      :: Int
    , cron       :: String
    , contentURL :: String
    } deriving Show

instance FromJSON Cron where
  parseJSON (Object x) = Cron
    <$> x .: "index"
    <*> x .: "cron"
    <*> x .: "contentURL"

instance ToJSON Cron where
  toJSON (Cron i c u) = object
    [ "index"      .= i
    , "cron"       .= c
    , "contentURL" .= u
    ]

readCronConfig :: IO B.ByteString
readCronConfig = do
  B.readFile "./env.json"

getCronJSON :: IO (Either String [Cron])
getCronJSON = eitherDecode <$> readCronConfig

twitterJob :: Cron -> IO ()
twitterJob crn = do
  updateWithAuthor (index crn)

loadJobs :: [Cron] -> IO ()
loadJobs cs = do
  tids <- execSchedule $ do
    mapM_ (\c -> addJob (twitterJob c) (T.pack . cron $ c)) cs
  print tids

runCrons :: IO ()
runCrons = do
  e <- getCronJSON
  case e of
    Left _   -> putStrLn "oh nos!"
    Right xs -> loadJobs xs
