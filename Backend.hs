{-# LANGUAGE OverloadedStrings #-}

-- TODO: * Write processRecurringBookings
--       * Write frontend which allows for admin of RecurringBooking's

module Backend (
    addRecurringBooking,
    removeRecurringBooking,
    getRecurringBookings,
    -- processRecurringBookings,
    -- debugging
    simpleRecBooking,
    manyRooms,
    fewPurposes,
    -- util
    removeAt
	) where

import Api
import Api.Util

-- Misc
import Data.Maybe
import Data.Time
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

-------------------------------------------------------------------------------
-- Types

data RecurringBooking = RecurringBooking {
  startDate :: Time,
  endDate :: Time,
  everyXWeeks :: Int,
  rooms :: [Room],
  purposes :: [Purpose],
  rPrivateComment :: PrivateComment,
  rPublicComment  :: PublicComment
} deriving (Show, Read)

-------------------------------------------------------------------------------
-- File IO

addRecurringBooking :: FilePath -> RecurringBooking -> IO ()
addRecurringBooking p b = do
  bs <- getRecurringBookings p
  let bs' = b : bs
  saveRecurringBookings p bs'

removeRecurringBooking :: FilePath -> Int -> IO ()
removeRecurringBooking p i = do
  bs <- getRecurringBookings p
  let bs' = removeAt i bs
  saveRecurringBookings p bs'

-- | Gets a list of RecurringBooking from a file
--   Returns [] if file does not exist
getRecurringBookings :: FilePath -> IO [RecurringBooking]
getRecurringBookings p = do
  handle <- openFile p ReadMode
  s <- hGetContents handle
  let bs  = if null s
            then []
            else read s :: [RecurringBooking]
  hClose handle
  return bs

-- | Saves a list of RecurringBooking to file.
--   Overwrites the file, if already exists
saveRecurringBookings :: FilePath -> [RecurringBooking] -> IO ()
saveRecurringBookings p bs = do
  handle <- openFile p WriteMode 
  hPutStr handle (show bs)
  hClose handle

-------------------------------------------------------------------------------
-- processRecurringBookings

-- Process the bookings in the given file,
-- create bookings for each as long as possible
-- TODO: make fn :: RecurringBooking -> Booking

processRecurringBookings :: FilePath -> IO ()
processRecurringBookings = undefined

-------------------------------------------------------------------------------
-- debugging data

simpleRecBooking :: RecurringBooking
simpleRecBooking = RecurringBooking {
  startDate = fromJust $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M" "2015-04-16 20:00",
  endDate   = fromJust $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M" "2015-04-16 22:00",
  everyXWeeks = 1,
  rooms = manyRooms,
  purposes = fewPurposes,
  rPrivateComment = "I'm not a robot",
  rPublicComment  = ""
}

manyRooms :: [Room]
manyRooms = [("205593.186","2109"),("192376.186","3215"),("192377.186","3217"),
  ("192381.186","4205"),("192382.186","4207"),("192383.186","5205"),
  ("192384.186","5207"),("192385.186","5209"),("192386.186","5211"),
  ("192387.186","5213"),("192388.186","5215"),("192389.186","5217"),
  ("192393.186","6205"),("192394.186","6207"),("192395.186","6209"),
  ("192396.186","6211"),("192397.186","6213"),("192398.186","6215"),
  ("205247.186","F4051"),("205248.186","F4052"),("205249.186","F4053"),
  ("205250.186","F4054"),("205251.186","F4055"),("205252.186","F4056"),
  ("205253.186","F4057"),("205254.186","F4058"),("192421.186","Idegr10"),
  ("192422.186","Idegr11"),("192429.186","Idegr4"),("192430.186","Idegr5")];

fewPurposes :: [Purpose]
fewPurposes = [("203460.192","Övrigt")]

-------------------------------------------------------------------------------
-- util functions

-- Removes the element at the specified index from the list
removeAt :: Int -> [a] -> [a]
removeAt i l = take i l ++ drop (i+1) l

