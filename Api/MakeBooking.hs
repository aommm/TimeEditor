{-# LANGUAGE OverloadedStrings #-}

-- TODO: fix error handling (=> fix exceptions)
-- Example now is incorrect


module Api.MakeBooking (
  getAvailableTimes,
  getAvailableRooms,
  getAvailablePurposes,
  makeBooking
  ) where

-- TimeEditor
import Api
import Api.Util
-- networking
import Network.Wreq
import qualified Network.Wreq.Types as WT
import qualified Network.Wreq.Session as S
import Network.HTTP.Types.Status (ok200)
import Network.HTTP.Client.Internal (HttpException (StatusCodeException))
-- Parsing
import Text.XML.HXT.Core hiding (trace)
import qualified Data.Tree.Class as T
import Text.HandsomeSoup
import Control.Lens hiding (deep, none)
-- misc
import Data.ByteString.Lazy (toStrict)
import Data.Text (pack, unpack, strip)
import Data.ByteString.UTF8 (toString)
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.Maybe (fromJust)
import qualified Control.Exception as E


-------------------------------------------------------------------------------
-- Get available times for booking

-- | Gets the time span in which it is possible to make bookings
getAvailableTimes :: S.Session -> IO (Maybe (Time,Time))
getAvailableTimes sess = do
    let mainUrl = "https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1"
    r <- S.get sess mainUrl
    let html = toString $ toStrict $ r ^. responseBody
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
                       -- TODO: add 23:59 to end date!
                       >>> ((arr fst >>> parseDate) &&& (arr snd >>> parseDate))
                       >>> arr makeAvailableTime
  where
    makeAvailableTime (a,b) = do
      a' <- a
      b' <- b
      let b'' = (23*3600+59*60+59) `addUTCTime` b'
      return (a',b'')

-- | Parses a date from a string formatted like "20150329"
parseDate :: IOSArrow String (Maybe Time)
parseDate = arr (parseTime defaultTimeLocale "%Y%m%d")


-------------------------------------------------------------------------------
-- Available rooms

-- | Get available rooms for booking, given a time interval.
--   Returns at most 30 rooms.
--   Assumes date portion of both times to be the same (since no bookings can last several days).
getAvailableRooms :: S.Session -> (Time,Time) -> IO ([Room])
getAvailableRooms s (start,end) = getObjects s params 0 100
 where
   params = [("types","186"),("subtypes","186"), ("starttime",startTime), ("endtime",endTime),("dates",dates)]
   dates      = formatTime defaultTimeLocale "%Y%m%d-%Y%m%d" start --"20150410-20150410"
   startTime  = formatTime defaultTimeLocale "%R" start -- "8:00"
   endTime    = formatTime defaultTimeLocale "%R" end   -- "9:00"

-------------------------------------------------------------------------------
-- Available purposes

-- | Get available purposes for booking a room
--   (As of now, only "Övrigt" is available)
getAvailablePurposes :: S.Session -> IO ([Purpose])
getAvailablePurposes s = getObjects s params 0 15
 where
   params = [("types","192"),("subtypes","192"), ("objects","192381.186")]

-------------------------------------------------------------------------------
-- Parse Objects (used for rooms and purposes)

type Object = (String,String)

-- | Gets available objects from objects.html given a parameter list.
--   Returns objects in interval [startIdx,endIdx]
--   Example: Use [0,15] to get the first 15 bookings
--   To specify what kind of objects you want, add "types" and "subtypes" as params
getObjects :: S.Session -> [(String,String)] -> Int -> Int -> IO [Object]
getObjects sess params startIdx endIdx
  | startIdx >= endIdx = return []
  | otherwise         = do
  r <- S.getWith opts sess url
  let html = toString $ toStrict $ r ^. responseBody
  -- debugSaveS ("roomsPage-"++show startIdx++".html") html
  objs     <- parseObjects html
  moreObjs <- getObjects sess params (startIdx+stepSize) endIdx
  return $ objs ++ moreObjs
  where
    stepSize   = 15 -- how many to fetch each time. only 15 appears to work
    url        = "https://se.timeedit.net/web/chalmers/db1/b1/objects?fr=t&partajax=t&im=f&add=f&sid=1002&l=sv_SE&step=1&grp=5"
    params'    = params ++ [("start",show startIdx), ("max",show stepSize), ("part","t"), ("media","html")]
    textParams = map (mapPair pack pack) params' -- Convert String->Text
    opts       = defaults { WT.params = textParams } -- Create Wreq options object

-- | Parses a list of strings from an HTML string
parseObjects :: String -> IO [Object]
parseObjects htmls = do
  let html = htmls `seq` parseHtml htmls
  times <- runX $ html >>> removeLinks >>> parseObjects'
  let stripper = (unpack.strip.pack)
      times' = map (mapPair stripper stripper) times
  return times'

-- | Removes div with "Fler resultat"-link from tree
removeLinks :: IOSArrow XmlTree XmlTree
removeLinks = processChildren (none `when` (hasAttrValue "class" (=="pageLinksWrap")))

-- TODO: parse ID also
parseObjects' :: IOSArrow XmlTree Object
parseObjects' = configSysVars (withTrace 1 : [])
                       -- >>> withTraceLevel 4 (traceDoc "resulting document")
                       >>> removeAllWhiteSpace
                       >>> getChildren
                       >>> deep parseObject


parseObject :: IOSArrow XmlTree Object
parseObject = this >>> (getAttrValue0 "data-id" &&& getAttrValue0 "data-name")




-------------------------------------------------------------------------------
-- makeBooking
-- 1198:
-- "https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1&step=2&id=-1&dates=20150406&datesEnd=20150406&startTime=0:00 &endTime=1:00 &o=192493.186,1198   &o=203460.192,Ãvrigt&nocache=3"
-- https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1&step=2&id=-1&dates=20150406&datesEnd=20150406&startTime=0:00&endTime=1:00&o=192493.186,1198&o=203460.192,Övrigt
-- https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1&step=2&id=-1&dates=20150406&datesEnd=20150406&startTime=0:00&endTime=1:00&o=192493.186,1198&o=203460.192,Övrigt&nocache=3


-- https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1&step=2

-- Idegr10:
-- "https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1&step=2&id=-1&dates=20150415&datesEnd=20150415&startTime=16:00&endTime=17:00&o=192421.186,Idegr10&o=203460.192,Ãvrigt&nocache=3"

makeBooking :: S.Session -> Booking -> IO Bool
makeBooking sess (Booking start end room purpose private public) = do
  E.catch postBookingRequest errorHandler
  where
    postBookingRequest = do 
      r <- S.postWith opts sess url dataa
      let status = r ^. responseStatus 
      return $ status == ok200
    errorHandler (StatusCodeException s _ _) = do
      putStr "Error! Http error in makeBooking: "
      print s
      return False
    params = [("","")]
    textParams = map (mapPair pack pack) params -- Convert String->Text
    opts   = defaults { WT.params = textParams } -- Create Wreq options object
    url    = "https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1&step=2"
    dataa  = ["fe2" := private,
              "fe8" := public,
              "dates" := formatTime defaultTimeLocale "%Y%m%d" start,
              "datesEnd" := formatTime defaultTimeLocale "%Y%m%d" end,
              "startTime" := formatTime defaultTimeLocale "%R" start,
              "endTime" := formatTime defaultTimeLocale "%R" end,
              "o" := fst room,
              "o" := fst purpose,
              "kind" := ("reserve"::String),
              "url" := url
              ]
