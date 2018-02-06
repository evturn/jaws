{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Jaws.System.Schedule where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import           Data.Monoid
import qualified Data.Text            as T
import           Jaws.Data            (runJaws)
import           Jaws.Internal        (fetchSource)
import           Jaws.Twitter.Author
import           Jaws.Twitter.Status
import           System.Cron

data Cron = Cron
    { index :: Int
    , cron  :: String
    } deriving Show

instance FromJSON Cron where
  parseJSON (Object x) = Cron
    <$> x .: "index"
    <*> x .: "cron"

instance ToJSON Cron where
  toJSON (Cron i c) = object
    [ "index"      .= i
    , "cron"       .= c
    ]

readCronConfig :: IO B.ByteString
readCronConfig = do
  B.readFile "./env.json"

getCronJSON :: IO (Either String [Cron])
getCronJSON = eitherDecode <$> readCronConfig

getSchedule :: Cron -> T.Text
getSchedule = T.pack . cron

loadJobs :: Cron -> IO ()
loadJobs c = do
  tids <- execSchedule $ do
    addJob (twitterJob $ index c) (getSchedule c)
  print tids

twitterJob :: Int -> IO ()
twitterJob index = do
  authors <- getJSON
  case authors of
    Left _   -> putStrLn "well, damn."
    Right as -> runUpdate (as !! index)

runUpdate :: Author -> IO ()
runUpdate author = do
  content <- fetchSource (contentURL author)
  status  <- runJaws content
  updateStatus (author, status)

runCrons :: IO ()
runCrons = do
  e <- getCronJSON
  case e of
    Left _   -> putStrLn "oh nos!"
    Right xs -> mapM_ loadJobs xs
