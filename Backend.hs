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
import Api.MakeBooking

-- Networking
import Network.Wreq
import qualified Network.Wreq.Session as S

-- Misc
import Control.Monad
import Data.Either
import Data.Maybe (fromJust)
import Data.Time
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

-------------------------------------------------------------------------------
-- Types

data RecurringBooking = RecurringBooking {
  rStartTime :: Time,
  rEndTime :: Time,
  everyXWeeks :: Integer,
  rooms :: [Room],
  purposes :: [Purpose],
  rPrivateComment :: PrivateComment,
  rPublicComment  :: PublicComment
} deriving (Show, Read)

-------------------------------------------------------------------------------
-- File IO

-- TODO: fails if file does not exist! (I.e. get fails)
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

-- TODO: * implement createBooking
--       * save changed RecurringBookings somehow? Where?

processRecurringBookings :: FilePath -> IO ()
processRecurringBookings p = S.withSession $ \sess -> do
    -- login
    putStrLn "logging in..."
    creds <- parseCredentials "credentials"
    login sess creds
    -- get bookings from recurringBookings
    putStrLn "get recurring bookings..."
    recBookings <- getRecurringBookings p
    putStrLn $ "number of recurring bookings: "++ show (length recBookings)
    putStrLn "createBookings for each..."
    bsAndRbs <- mapM (createBookings sess) recBookings
    let bookings     = concatMap fst bsAndRbs
        recBookings' = map snd bsAndRbs
    -- save changed recurring bookings
    putStrLn "save new recurring bookings..."
    -- saveRecurringBookings p recBookings'
    -- try to place bookings
    putStrLn "place bookings..."
    success <- mapM (makeBooking sess) bookings
    print success
    putStrLn "done!"
    return () -- TODO: return something meaningful?

-- | Creates as many Bookings as possible from a RecurringBooking.
--   Also returns a changed RecurringBooking
createBookings :: S.Session -> RecurringBooking -> IO ([Booking],RecurringBooking)
createBookings sess rb = do
  m <- createBooking sess rb
  case m of
    Right rb'    -> return ([],rb')
    Left (b,rb') -> do
      (bs,rbs) <- createBookings sess rb'
      return $ (b:bs, rbs)

-- Creates as many Booking objects as possible from a RecurringBooking.
-- When no more Bookings can be created, returns only the RecurringBooking
createBooking :: S.Session -> RecurringBooking -> IO (Either (Booking,RecurringBooking) RecurringBooking)
createBooking sess rb = do
  -- Assume HTTP works ok
  (Just (sTime,eTime)) <- getAvailableTimes sess
  if (rStartTime rb > eTime)
  then return $ Right rb
  else do
    let xWeeks = fromInteger $ 60*60*24*7*(everyXWeeks rb)
        times = iterate (addUTCTime xWeeks) (rStartTime rb)
        notTooLate = takeWhile (<= eTime) times
        notTooSoon = dropWhile (< sTime) notTooLate
        thisStartTime = head notTooSoon
        nextStartTime = xWeeks `addUTCTime` thisStartTime
    putStrLn "Termination"
    -- print $ take 10 times
    print eTime
    print $ take 3 times
    print notTooLate
    print notTooSoon
    print thisStartTime
    print nextStartTime
    -- TODO: Fix MakeBooking.hs
    --       Continue with rooms etc
    return $ Right rb

  -- if (rStartTime rb) `inRange` times
  -- then undefined
  -- else undefined



-- getAvailableTimes :: S.Session -> IO (Maybe (Time,Time))

-------------------------------------------------------------------------------
-- debugging data

simpleRecBooking :: RecurringBooking
simpleRecBooking = RecurringBooking {
  rStartTime = fromJust $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M" "2015-04-22 20:00",
  rEndTime   = fromJust $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M" "2015-04-22 22:00",
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

-- | The 'concatMapM' function generalizes 'concatMap' to arbitrary monads.
concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

-- | Checks if a is in the range [b,c] (inclusive)
inRange :: Ord a => a -> (a,a) -> Bool
inRange a (b,c) = a >= b && a <= c