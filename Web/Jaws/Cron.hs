{-# LANGUAGE OverloadedStrings #-}

module Web.Jaws.Cron where

import           Data.Monoid
import           System.Cron

cronaldMcCronald :: IO ()
cronaldMcCronald = do
  putStrLn "\nPlease try our salads\n"
  let Right cs1 = parseCronSchedule "*/2 * 3 * 4,5,6"
  print $ describe defaultOpts cs1

  let Right cs2 = parseCronSchedule "*/2 12 3 * 4,5,6"
  print $ describe (twentyFourHourFormat <> verbose) cs2

  let Right cs3 = parseCronSchedule "00 09,14,22 * * *"
  print $ describe (twentyFourHourFormat <> verbose) cs3

  let Right cs4 = parseCronSchedule "00 00,09,16 * * *"
  print $ describe (twentyFourHourFormat <> verbose) cs4

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
