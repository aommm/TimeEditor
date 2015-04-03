module Api.MakeBooking () where

-- TimeEditor
import Api
import Api.Util
-- networking
import Network.Wreq
import qualified Network.Wreq.Session as S
-- Parsing
import Text.XML.HXT.Core hiding (trace)
import qualified Data.Tree.Class as T
import Text.HandsomeSoup
import Control.Lens hiding (deep, none)
-- misc
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Time
import System.Locale (defaultTimeLocale)

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

getAvailableRooms :: S.Session -> (Time,Time) -> IO ([Room])
getAvailableRooms sess (start,end) = do
  let dates = "20150410-20150410"
      startTime = "8%3A00"
      endTime   = "9%3A00"
      url = "https://se.timeedit.net/web/chalmers/db1/b1/objects.html?max=15&fr=t&partajax=t&im=f&add=f&sid=1002&l=sv_SE&step=1&grp=5&types=186&subtypes=186&dates="++ dates ++ "&starttime="++ startTime ++ "&endtime="++endTime
  return []

-------------------------------------------------------------------------------
-- Debugging
main :: IO ()
main = S.withSession $ \sess -> do
    -- timeFun
    creds <- parseCredentials "credentials"
    login sess creds
    times <- getAvailableTimes sess
    print times

