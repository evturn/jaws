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
  path <- (++"/src/dev/jaws/env.json") <$> getEnv "HOME"
  B.readFile path

getCronJSON :: IO (Either String [Cron])
getCronJSON = eitherDecode <$> readCronConfig

loadJob x = do
  xs <- jaws "url" $ contentURL x
  updateStatus (index x)

loadCron xs = do
  return $ fmap (\x ->
    addJob (loadJob x) (T.pack . cron $ x)) xs
  return ()

doIt = do
  e <- getCronJSON
  case e of
    Left _   -> putStrLn "no no NO!"
    Right xs -> loadCron xs


cronSimmons :: IO ()
cronSimmons = do
   tids <- execSchedule $ do
       addJob job1 "* * * * *"
       addJob job2 "0 * * * *"
   print tids

job1 :: IO ()
job1 = putStrLn "Job 1"

job2 :: IO ()
job2 = putStrLn "Job 2"

