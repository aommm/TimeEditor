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
import Text.HandsomeSoup
import qualified Data.Tree.Class as T

import Debug.Trace

-------------------------------------------------------------------------------
-- Types

-- A room booking
data Booking = Booking StartTime EndTime Room
    deriving (Eq, Show)
type StartTime = UTCTime
type EndTime   = UTCTime
type Room      = String

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
parseBookings _htmls = do
    -- print htmls
    htmls <- readFile "exampleInput.html"
    
    let html  = htmls `seq` parseHtml htmls
    bookings <- runX $ html >>> parseBookings'
    print $ bookings
    print $ length bookings
    return []

-- css :: ArrowXml a => [Char] -> a XmlTree XmlTree 

-- xshow :: a n XmlTree -> a n String

-- TODO: parse into booking objects later. Persons for now
--parseBookings' :: ArrowXml a => a XmlTree [Booking]
parseBookings' :: IOSArrow XmlTree [Person]
parseBookings' =    configSysVars (withTrace 1 : [])
                    >>> css "#texttable"
                    -- >>> withTraceLevel 4 (traceDoc "resulting document")
                    >>> getChildren >>> hasName "table"
                    -- >>> withTraceLevel 4 (traceDoc "resulting document")
                     -- >>> arr (\t -> Left ([],t)) >. (show.length)
                    >>> arr (\t -> Left ([],t))
                    >>> parseAndRemoves 
                    >>> arr (\r -> fromRight r )


-- Dummy data type
data Person = Person String String
    deriving Show

--  arr :: (b -> c) -> a b c 
-- (&&&) :: a b c -> a b c' -> a b (c, c') 
-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')
-- ifA   :: a b c -> a b d -> a b d -> a b d

parseAndRemoves :: IOSArrow (Either ([Person],XmlTree) [Person]) (Either ([Person],XmlTree) [Person])
parseAndRemoves = 
    traceMsg 1 "parseAndRemoves" >>>
    ifA (isA isLeft) trueArr falseArr
    where
        trueArr =  traceMsg 1 "trueArr: 1" 
                   >>> arr fromLeft
                   >>> traceMsg 1 "trueArr: 2"
                   >>> ((arr snd
                        >>> removeAllWhiteSpace -- remove nodes: XText "\n\t\t"
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

parseRowX :: IOSArrow XmlTree (Maybe Person)
parseRowX = 
            traceMsg 1 "parseRowX"
            >>> ((this          -- is a table
                >>> getChildren -- get rows
                >>. take 1
                )
                >>> multi (hasName "td") -- get all <td>s text
                >>> deep getText
                )
            >. makePerson
            >>> traceMsg 1 "parseRowX: made person"
            >>> traceValue 1 show


    where makePerson [n,a] = Just $ Person n a
          makePerson _     = Nothing


-- getNode :: Tree t => a (t b) b
-- getChildren :: Tree t => a (t b) (t b)
-- mkTree :: Tree t => b -> [t b] -> a c (t b)
-- arr2 :: (b1 -> b2 -> c) -> a (b1, b2) c
-- arrL :: (b -> [c]) -> a b c
-- listA :: a b c -> a b [c]

-- (&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')
-- (>>>) :: Control.Category.Category cat => cat a b -> cat b c -> cat a c

parseRowTree :: IOSArrow XmlTree XmlTree
parseRowTree = traceMsg 1 "parseRowTree"
               >>>
               getNode -- get tree root
               &&&
               (listA (getChildren >>. drop 2)) -- get tree children, throw away 2
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

myDateParser :: String -> UTCTime
myDateParser s = let blubb = parseTime defaultTimeLocale "%F" s in
                 case blubb of
                     Just d  -> d
                     Nothing -> error "myDateParser: couldn't parse date"

-- OK time works. Leave this for now
timeFun :: IO ()
timeFun = do
    print "hej"


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


-- response :: IO String
-- response =
--     do
--       (_, rsp)
--          <- Network.Browser.browse $ do
--                setAllowRedirects True -- handle HTTP redirects
--                request $ getRequest "http://www.haskell.org/"
--       return (take 100 (rspBody rsp))
