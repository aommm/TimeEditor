{-# LANGUAGE OverloadedStrings #-}

module Api.MakeBooking (
  getAvailableTimes,
  getAvailableRooms,
  getAvailablePurposes
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
-- Available rooms

-- | Get available rooms for booking, given a time interval.
--   Returns at most 30 rooms.
--   Assumes date portion of both times to be the same (since no bookings can last several days).
getAvailableRooms :: S.Session -> (Time,Time) -> IO ([Room])
getAvailableRooms s (start,end) = getObjects s params 0 30
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

-- | Gets available objects from objects.html given a parameter list.
--   Returns objects in interval [startIdx,endIdx]
--   Example: Use [0,15] to get the first 15 bookings
--   To specify what kind of objects you want, add "types" and "subtypes" as params
getObjects :: S.Session -> [(String,String)] -> Int -> Int -> IO [String]
getObjects sess params startIdx endIdx
  | startIdx >= endIdx = return []
  | otherwise         = do
  r <- S.getWith opts sess url
  let html = C8.unpack $ r ^. responseBody
  -- debugSaveS ("roomsPage-"++show startIdx++".html") html
  objs     <- parseObjects html
  moreObjs <- getObjects sess params (startIdx+stepSize) endIdx
  return $ objs ++ moreObjs
  where
    stepSize   = 15 -- how many to fetch each time. only 15 appears to work
    url        = "https://se.timeedit.net/web/chalmers/db1/b1/objects.html?max=15&fr=t&partajax=t&im=f&add=f&sid=1002&l=sv_SE&step=1&grp=5"
    params'    = params ++ [("start",show startIdx), ("max",show stepSize)]
    textParams = map (mapPair pack pack) params' -- Convert String->Text
    opts       = defaults { WT.params = textParams } -- Create Wreq options object

-- | Parses a list of strings from an HTML string
parseObjects :: String -> IO [String]
parseObjects htmls = do
  let html = htmls `seq` parseHtml htmls
  times <- runX $ html >>> removeLinks >>> parseObjects'
  let times' = map (unpack.strip.pack) times
  return $ times'

-- | Removes div with "Fler resultat"-link from tree
removeLinks :: IOSArrow XmlTree XmlTree
removeLinks = processChildren (none `when` (hasAttrValue "class" (=="pageLinksWrap")))

parseObjects' :: IOSArrow XmlTree String
parseObjects' = configSysVars (withTrace 1 : [])
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
    purposes <- getAvailablePurposes sess
    putStrLn $ "available times:"++show times
    putStrLn $ "available rooms:"++show rooms
    putStrLn $ "available purposes:"++show purposes
    putStrLn $ purposes !! 0

