{-# LANGUAGE OverloadedStrings #-}

module Api.MakeBooking (
  getAvailableTimes,
  getAvailableRooms
  ) where

-- TimeEditor
import Api
import Api.Util
-- networking
import Network.Wreq
import qualified Network.Wreq.Types as WT
import qualified Network.Wreq.Session as S
-- Parsing
import Text.XML.HXT.Core hiding (trace)
import qualified Data.Tree.Class as T
import Text.HandsomeSoup
import Control.Lens hiding (deep, none)
-- misc
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text (pack, unpack, strip)
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.Maybe (fromJust)

-------------------------------------------------------------------------------
-- TODO list

getAvailablePurposes :: S.Session -> IO ([Purpose])
getAvailablePurposes = undefined

makeBooking :: S.Session -> Booking -> IO Bool
makeBooking = undefined

-------------------------------------------------------------------------------
-- Get available times for booking

-- | Gets the time span in which it is possible to make bookings
getAvailableTimes :: S.Session -> IO (Maybe (Time,Time))
getAvailableTimes sess = do
    let mainUrl = "https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1"
    r <- S.get sess mainUrl
    let html = C8.unpack $ r ^. responseBody
    debugSaveS "reservePage.html" html
    parseAvailableTimes html


parseAvailableTimes :: String -> IO (Maybe (Time,Time))
parseAvailableTimes htmls = do
  let html = htmls `seq` parseHtml htmls
  times <- runX $ traceMsg 1 "a" >>> html >>> parseAvailableTimes' -- Never run!
  if   length times > 0
  then return $ head times
  else return Nothing

parseAvailableTimes' :: IOSArrow XmlTree (Maybe (Time,Time))
parseAvailableTimes' = configSysVars (withTrace 1 : [])
                       >>> css "#datepickerDirect"
                       >>> (getAttrValue "data-min" &&& getAttrValue "data-max")
                       >>> ((arr fst >>> parseDate) &&& (arr snd >>> parseDate))
                       >>> arr makeAvailableTime
  where
    makeAvailableTime (a,b) = do
      a' <- a
      b' <- b
      return (a',b')

-- | Parses a date from a string formatted like "20150329"
parseDate :: IOSArrow String (Maybe Time)
parseDate = arr (parseTime defaultTimeLocale "%Y%m%d")


-------------------------------------------------------------------------------
-- Get available rooms for booking, given a time interval

-- TODO:
-- print Time objects appropriately
-- Use getWith params instead of concatenating
-- getWith :: Options -> Session -> String -> IO (Response ByteString)
-- https://hackage.haskell.org/package/wreq-0.1.0.1/docs/Network-Wreq-Session.html


-- | Gets at most 30 rooms available for booking for a certain time interval.
--   Assumes date portion of both times to be the same (since no bookings can last several days)
getAvailableRooms :: S.Session -> (Time,Time) -> IO ([Room])
getAvailableRooms s ts = getAvailableRooms' s ts 0 30


-- | Gets available rooms for booking for a certain time interval.
--   Assumes date portion of both times to be the same (since no bookings can last several days)

--   Returns bookings whose server indices are in interval [startIdx,endIdx]
--   Example: Use [0,15] to get the first 15 bookings
getAvailableRooms' :: S.Session -> (Time,Time) -> Int -> Int -> IO ([Room])
getAvailableRooms' sess (start,end) startIdx endIdx 
  | startIdx >= endIdx = return []
  | otherwise         = do
  r <- S.getWith opts sess url
  let html = C8.unpack $ r ^. responseBody
  -- debugSaveS ("roomsPage-"++show startIdx++".html") html
  rooms     <- parseAvailableRooms html
  moreRooms <- getAvailableRooms' sess (start,end) (startIdx+stepSize) endIdx
  return $ rooms ++ moreRooms
  where
    dates      = formatTime defaultTimeLocale "%Y%m%d-%Y%m%d" start --"20150410-20150410"
    startTime  = formatTime defaultTimeLocale "%R" start -- "8:00"
    endTime    = formatTime defaultTimeLocale "%R" end   -- "9:00"
    stepSize   = 15 -- how many to fetch each time. only 15 appears to work
    url        = "https://se.timeedit.net/web/chalmers/db1/b1/objects.html?fr=t&partajax=t&im=f&add=f&sid=1002&l=sv_SE&step=1&grp=5&types=186&subtypes=186"
    params     = [("start",show startIdx), ("max",show stepSize), ("dates",dates), ("starttime",startTime),("endtime",endTime)]
    textParams = map (mapSnd pack) params -- Convert String->Text
    opts       = defaults { WT.params = textParams } -- Create Wreq options object

-- | Parses a list of rooms from an HTML string
parseAvailableRooms :: String -> IO [Room]
parseAvailableRooms htmls = do
  let html = htmls `seq` parseHtml htmls
  times <- runX $ html >>> removeLinks >>> parseAvailableRooms'
  let times' = map (unpack.strip.pack) times
  return $ times'

-- | Removes div with "Fler resultat"-link from tree
removeLinks :: IOSArrow XmlTree XmlTree
removeLinks = processChildren (none `when` (hasAttrValue "class" (=="pageLinksWrap")))

parseAvailableRooms' :: IOSArrow XmlTree String
parseAvailableRooms' = configSysVars (withTrace 1 : [])
                       -- >>> withTraceLevel 4 (traceDoc "resulting document")
                       >>> removeAllWhiteSpace
                       >>> getChildren
                       >>> deep getText

-------------------------------------------------------------------------------
-- Debugging
main :: IO ()
main = S.withSession $ \sess -> do
    -- timeFun
    creds <- parseCredentials "credentials"
    login sess creds
    times <- getAvailableTimes sess
    let (t1,_) = fromJust times
        oneHour = 3600
        t2 = addUTCTime oneHour t1
    rooms <- getAvailableRooms sess (t1,t2)
    putStrLn $ "available times:"++show times
    putStrLn $ "available rooms:"++show rooms

