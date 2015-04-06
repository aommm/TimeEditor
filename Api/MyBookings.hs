{-# LANGUAGE OverloadedStrings #-}

-- TODO: Parse more information from bookings
-- (Ids and so on)

-- If not present, separate types for listing and making bookings

module Api.MyBookings (
    getBookings
  ) where

import Api
import Api.Util

-- Networking
import Network.Wreq
import qualified Network.Wreq.Session as S

-- Parsing
import Text.XML.HXT.Core hiding (trace)
import qualified Data.Tree.Class as T
import Text.HandsomeSoup
import Control.Lens hiding (deep, none)

import Data.List.Split (splitOn)
import Data.Either.Unwrap
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Time
import System.Locale -- locale needed for time


import Debug.Trace

-------------------------------------------------------------------------------
-- Operations

getBookings :: S.Session -> IO ([Booking])
getBookings sess = do
    let mainUrl = "https://se.timeedit.net/web/chalmers/db1/b1/my.html?h=t&sid=1002&g=f"
    r2 <- S.get sess mainUrl
    let html = C8.unpack $ r2 ^. responseBody
    debugSaveS "bookingsPage.html" html
    bookings <- parseBookings html
    return $ bookings

-- fromLA :: ArrowList a => LA b c -> a b c
-- xread :: ArrowXml a => a String XmlTree
-- runX :: IOSArrow XmlTree c -> IO [c]
-- parseHtml :: String -> IOSArrow b (NTree XNode) 

-------------------------------------------------------------------------------
-- Parsing

parseBookings :: String -> IO [Booking]
parseBookings htmls = do
    let html  = htmls `seq` parseHtml htmls
    -- htmls <- readFile "exampleInput.html"

    bookings <- runX $ html >>> parseBookings'
    print $ bookings
    print $ length bookings
    return []

-- css :: ArrowXml a => [Char] -> a XmlTree XmlTree 

-- xshow :: a n XmlTree -> a n String

-- TODO: parse into booking objects later. Persons for now
--parseBookings' :: ArrowXml a => a XmlTree [Booking]
parseBookings' :: IOSArrow XmlTree [Booking]
parseBookings' =    configSysVars (withTrace 1 : [])
                    >>> css "#texttable"
                    -- >>> withTraceLevel 4 (traceDoc "resulting document")
                    >>> getChildren >>> hasName "table"
                    -- >>> processChildren (this >>. drop 1)
                    >>> removeAllWhiteSpace -- remove nodes: XText "\n\t\t"
                    >>> processChildrenList (drop 2) -- remove first two rows
                    -- >>> printDoc
                    >>> processChildren (this)
                    -- >>> withTraceLevel 4 (traceDoc "resulting document")
                     -- >>> arr (\t -> Left ([],t)) >. (show.length)
                    >>> arr (\t -> Left ([],t))
                    >>> parseAndRemoves 
                    >>> arr (\r -> fromRight r )

printDoc = withTraceLevel 4 (traceDoc "resulting document")
-- printVal :: IOSArrow XmlTree XmlTree 
-- printVal = traceValue 1 show
-- printMsg :: String -> IOSArrow XmlTree XmlTree 
printMsg = traceMsg 1
--  arr :: (b -> c) -> a b c 
-- (&&&) :: a b c -> a b c' -> a b (c, c') 
-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')
-- ifA   :: a b c -> a b d -> a b d -> a b d

parseAndRemoves :: IOSArrow (Either ([Booking],XmlTree) [Booking]) (Either ([Booking],XmlTree) [Booking])
parseAndRemoves = 
    traceMsg 1 "parseAndRemoves" >>>
    ifA (isA isLeft) trueArr falseArr
    where
        trueArr =  traceMsg 1 "trueArr: 1" 
                   >>> arr fromLeft
                   >>> traceMsg 1 "trueArr: 2"
                   >>> ((arr snd
                        -- >>> withTraceLevel 4 (traceDoc "resulting doc") 
                        >>> ifExists (parseRowX &&& parseRowTree)
                        >>> traceMsg 1 "rowx and tree done")
                   &&& (
                    arr fst
                    >>> this
                    )
                   )
                   >>> traceMsg 1 "trueArr: 3"
                   -- >>> traceValue 1 show
                   >>> ifA (isA parseSuccess) continue end
                   >>> traceMsg 1 "trueArr: 4"
        parseSuccess (Just (Just x,  t), xs) = trace "parseSuccess: true"  $ True
        parseSuccess (Just (Nothing, t), xs) = trace "parseSuccess: false" $ False -- TODO nothing
        parseSuccess (Nothing,           xs) = trace "parseSuccess: false" $ False -- TODO nothing
        continue =  traceMsg 1 "continue" >>>
                    arr (\(Just (Just x, t), xs) -> Left (x:xs,t))
                    >>> parseAndRemoves
        end = traceMsg 1 "end" >>>
              arr (\(Nothing, xs) -> Right xs)
              -- arr (\(Just (Nothing, _t), xs) -> Right xs)

        falseArr = traceMsg 1 "falseArr" >>>
            this

-- | If the given arrow evaluates to a result, returns Just that result, otherwise Nothing
--   Useful for parsing, if we're not sure whether a selector arrow will succeed or not.
--   By using this, you can check for Nothing, and in that case supply a default value.
ifExists :: ArrowIf a => a b c -> a b (Maybe c)
ifExists f = ifThenKeepResult f (arr Just) (arr $ const Nothing)

-- | Evaluates an expression and passes the resulting value into t if it's not empty.
--   If it is empty, runs f instead.
ifThenKeepResult :: ArrowIf a => a b c -> a c d -> a () d -> a b d
ifThenKeepResult c t f  = ifA c (c >>> t) (arr (\_ -> ()) >>> f)

-- (>>.) :: a b c -> ([c] -> [d]) -> a b d
-- (>.)  :: a b c -> ([c] -> d)   -> a b d
-- getChildren :: (Tree t, ArrowTree a) => a (t b) (t b)

parseRowX :: IOSArrow XmlTree (Maybe Booking)
parseRowX = 
            traceMsg 1 "parseRowX"
            -- >>> withTraceLevel 4 (traceDoc "resulting doc")
            -- >>> getChildren 
            -- >>> withTraceLevel 4 (traceDoc "resulting doc")
            >>> ((getChildren >>. take 1) >>> parseDateTr)
                &&&
                (((getChildren >>. drop 1) >>. take 1) >>> parseRowTr) -- TODO: why can't do drop and take at the same time?
            -- >>> traceValue 1 show
            >>> arr (uncurry setBookingDay)
            >>> traceMsg 1 "parseRowX: made booking"
            >>> traceValue 1 show


-- big TODO
parseDateTr :: IOSArrow XmlTree (Maybe UTCTime)
parseDateTr = 
  -- withTraceLevel 4 (traceDoc "parseDateTr: resulting doc")
  dropChildren 1 >>> takeChildren 1
  >>> deep getText
  -- >>> traceMsg 1 "parseDateTr. Date string:"
  -- >>> traceValue 1 show
  >>> arr myDateParser

parseRowTr :: IOSArrow XmlTree (Maybe Booking)
parseRowTr = -- withTraceLevel 4 (traceDoc "parseRowTr: resulting doc")
             (dropChildren 1 -- remove first <td>
             >>> deep getText -- (doesn't return "" if (comment) <td> is empty!)
             ) 
             >. makeBooking
    where makeBooking (timeTd:roomTd:rest) = do  -- TODO: rest may contain an additional element: the comment for the booking
                                                (start,end) <- myTimeParser timeTd
                                                room <- myRoomParser roomTd
                                                let emptyStr = "" :: String
                                                return $ Booking start end room (emptyStr,emptyStr) emptyStr emptyStr -- TODO
          makeBooking _ = Nothing


-- | Sets the day of the 'booking' to be the day in the 'date'.
setBookingDay :: Maybe UTCTime -> Maybe Booking -> Maybe Booking
setBookingDay date booking = do
  b <- booking -- throw away date, but get time
  d <- date
  let newStart = (startTime b) {utctDay = utctDay d}
  let newEnd   = (endTime   b) {utctDay = utctDay d}
  return b {startTime=newStart, endTime = newEnd}




-- getNode :: Tree t => a (t b) b
-- getChildren :: Tree t => a (t b) (t b)
-- mkTree :: Tree t => b -> [t b] -> a c (t b)
-- arr2 :: (b1 -> b2 -> c) -> a (b1, b2) c
-- arrL :: (b -> [c]) -> a b c
-- listA :: a b c -> a b [c]

-- (&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')
-- (>>>) :: Control.Category.Category cat => cat a b -> cat b c -> cat a c

-- TODO: program is run in weird ways - if we have several times on the same date, the first is never parsed


parseRowTree :: IOSArrow XmlTree XmlTree
parseRowTree = traceMsg 1 "parseRowTree"
               -- >>> withTraceLevel 4 (traceDoc "resulting doc")
               >>> dropChildrenAt 1 1
               -- >>> withTraceLevel 4 (traceDoc "taken 5!")
               >>> ifA trIsTheLast none' this' -- TODO: always evaluates first argument =(
               -- >>> withTraceLevel 4 (traceDoc "taken and dropped!")
  where none' = traceMsg 1 "NONEe" >>> dropChildren 1
        this' = traceMsg 1 "THIS" >>> this


-- trIsTheLast :: IOSArrow XmlTree XmlTree
trIsTheLast = listA getChildren >>> isA isSingleton --arr isSingleton >>> traceValue 1 show
  -- neg (hasAttr "class")


-------------------------------------------------------------------------------
-- Utilities and misc parsing

-- | Parses two times from a string formatted like "12:00 - 13:00"
myTimeParser :: String -> Maybe (UTCTime,UTCTime)
myTimeParser s = do
  let [s1,s2] = splitOn "-" s
  start <- parseTime defaultTimeLocale "%R" s1
  end   <- parseTime defaultTimeLocale "%R" s2
  return (start,end)

-- TODO: A room is always presented as "x, y". Decide which of x and y is important
--       and return only that here
myRoomParser :: String -> Maybe Room
myRoomParser s = return ("",s)

-- | Parses a date from a string formatted like "anytext%here 2015-03-29"
myDateParser :: String -> Maybe UTCTime
myDateParser s = do
  let [s1,s2] = splitOn " " s
  parseTime defaultTimeLocale "%F" s2
