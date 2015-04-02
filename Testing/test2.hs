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
-- Main program

main :: IO ()
main = S.withSession $ \sess -> do
    doTest sess


doTest :: S.Session -> IO ()
doTest sess = do
    test1 

-- fromLA :: ArrowList a => LA b c -> a b c
-- xread :: ArrowXml a => a String XmlTree
-- runX :: IOSArrow XmlTree c -> IO [c]
-- parseHtml :: String -> IOSArrow b (NTree XNode) 

-------------------------------------------------------------------------------
-- Parsing

test1 :: IO ()
test1 = do
    
    htmls <- readFile "exampleInput2.html"
    let html  = htmls `seq` parseHtml htmls

    bookings <- runX $ html >>> test1'
    print $ bookings
    

-- css :: ArrowXml a => [Char] -> a XmlTree XmlTree 

-- xshow :: a n XmlTree -> a n String


--test1' :: ArrowXml a => a XmlTree [Booking]
test1' :: IOSArrow XmlTree String
test1' =    configSysVars (withTrace 1 : [])
                    >>> ifA myProg (arr (\_->"hej")) (arr (\_->"då")) 


myProg :: IOSArrow XmlTree XmlTree
myProg = deep (hasName "table")
                    >>> getChildren
                    >>> hasName "tr"
                    >>> getChildren
