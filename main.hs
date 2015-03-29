{-# LANGUAGE OverloadedStrings #-}

import Data.Time
import Data.Either.Unwrap
import System.Locale -- locale needed for time
import System.IO
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8

import Control.Lens hiding (deep)

-- Networking
import Network.Wreq
import qualified Network.Wreq.Session as S

-- Parsing
import Text.XML.HXT.Core hiding (trace)
import qualified Data.Tree.Class as T
import Text.HandsomeSoup

import Data.List.Split (splitOn)

import Debug.Trace

-------------------------------------------------------------------------------
-- Types

-- A room booking
data Booking = Booking {startTime :: Time, endTime :: Time, room :: Room}
    deriving (Eq, Show)
type Time = UTCTime
type Room = String

-- Login credentials
type Credentials = (Username,Password)
type Username    = String
type Password    = String

-------------------------------------------------------------------------------
-- Main program

main :: IO ()
main = S.withSession $ \sess -> do
    -- timeFun
    creds <- parseCredentials "credentials"
    login sess creds
    bookings <- getBookings sess
    print bookings


login :: S.Session -> Credentials -> IO ()
login sess (user,pass) = do
    let loginUrl = "https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1"
    let loginData = ["authServer" := ("student" :: String), "username" := user, "password" := pass]
    r <- S.post sess loginUrl loginData
    return ()


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
                    >>> printDoc
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
                        >>> (parseRowX &&& parseRowTree)
                        >>> traceMsg 1 "rowx and tree done")
                   &&& (
                    arr fst
                    >>> this
                    )
                   )
                   >>> traceMsg 1 "trueArr: 3"
                   >>> ifA (isA parseSuccess) continue end
                   >>> traceMsg 1 "trueArr: 4"
        parseSuccess ((Just x,  t), xs) = trace "parseSuccess: true"  $ True
        parseSuccess ((Nothing, t), xs) = trace "parseSuccess: false" $ False
        continue =  traceMsg 1 "continue" >>>
                    arr (\((Just x, t), xs) -> Left (x:xs,t))
                    >>> parseAndRemoves
        end = traceMsg 1 "end" >>>
              arr (\((Nothing, _t), xs) -> Right xs)

        falseArr = traceMsg 1 "falseArr" >>>
            this


-- (>>.) :: a b c -> ([c] -> [d]) -> a b d
-- (>.)  :: a b c -> ([c] -> d)   -> a b d
-- getChildren :: (Tree t, ArrowTree a) => a (t b) (t b)

parseRowX :: IOSArrow XmlTree (Maybe Booking)
parseRowX = 
            traceMsg 1 "parseRowX"
            >>> ((getChildren >>. take 1) >>> parseDateTr)
                &&&
                ((getChildren >>. take 1 . drop 1) >>> parseRowTr)
            >>> arr (uncurry setBookingDay)
            >>> traceMsg 1 "parseRowX: made booking"
            >>> traceValue 1 show


-- big TODO
parseDateTr :: IOSArrow XmlTree (Maybe UTCTime)
parseDateTr = 
  -- withTraceLevel 4 (traceDoc "resulting doc")
  dropChildren 1 >>> takeChildren 1
  >>> deep getText
  -- >>> traceMsg 1 "parseDateTr. Date string:"
  -- >>> traceValue 1 show
  >>> arr myDateParser

parseRowTr :: IOSArrow XmlTree (Maybe Booking)
parseRowTr = (dropChildren 1 -- remove first <td>
             >>> deep getText) 
             >. makeBooking
    where makeBooking [timeTd,roomTd,textTd] = do 
                                                (start,end) <- myTimeParser timeTd
                                                room <- myRoomParser roomTd
                                                return $ Booking start end room
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


-- TODO next: make this change the tree in appropriate ways
-- Quite hard?
-- Skip <tr> with date info, throw away <tr> with time info.
-- If no other left in this <tr> with date info, remove it
parseRowTree :: IOSArrow XmlTree XmlTree
parseRowTree = traceMsg 1 "parseRowTree"
               >>> withTraceLevel 4 (traceDoc "resulting doc")
               >>>
               (getNode -- get tree root
               &&&
               (listA (getChildren >>. drop 1))) -- get tree children, throw away 2
               >>> arr2 T.mkTree -- construct new tree


dropChildren :: Int -> IOSArrow XmlTree XmlTree 
dropChildren i = processChildrenList (drop i)

takeChildren :: Int -> IOSArrow XmlTree XmlTree 
takeChildren i = processChildrenList (take i)

-- | Modifies a tree by running a function on the list of children
processChildrenList :: ([XmlTree] -> [XmlTree]) -> IOSArrow XmlTree XmlTree
processChildrenList f = traceMsg 1 "parseRowTree"
               >>>
               getNode -- get tree root
               &&&
               (listA (getChildren >>. f)) -- get tree children, throw away 2
               >>> arr2 T.mkTree -- construct new tree


-------------------------------------------------------------------------------
-- Utilities and misc parsing

parseCredentials :: String -> IO Credentials
parseCredentials path = do
    h <- openFile path ReadMode
    user <- hGetLine h
    pass <- hGetLine h
    hClose h
    return (user,pass)

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
myRoomParser s = return s

-- | Parses a date from a string formatted like "anytext%here 2015-03-29"
myDateParser :: String -> Maybe UTCTime
myDateParser s = do
  let [s1,s2] = splitOn " " s
  parseTime defaultTimeLocale "%F" s2


-- Save a string to "hej.html"
debugSaveS :: FilePath -> String -> IO ()
debugSaveS path a = do
    file <- openFile path ReadWriteMode
    hPutStr file a
    hClose file

-- Save a lazy bytestring to "hej.html"
debugSaveBS :: FilePath -> BS.ByteString -> IO ()
debugSaveBS path a = do
    file <- openFile path ReadWriteMode
    BS.hPut file a
    hClose file
