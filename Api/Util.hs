module Api.Util (
  -- Parsing
  dropChildren,
  takeChildren,
  dropChildrenAt,
  processChildrenList,
  -- Misc
  parseCredentials,
  isSingleton,
  debugSaveS,
  debugSaveBS,
  mapFst,
  mapSnd
  )
  where

import Api

-- Parsing
import Text.XML.HXT.Core hiding (trace)
import qualified Data.Tree.Class as T
import Text.HandsomeSoup
import Control.Lens hiding (deep, none)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import System.IO

-------------------------------------------------------------------------------
-- Parsing utilities

-- | Drops n children, beginning at position i
dropChildrenAt :: Int -> Int -> IOSArrow XmlTree XmlTree 
dropChildrenAt i n = processChildrenList dropAt
  where dropAt l = take i l ++ take ((length l)-n-i) (drop (i+n) l)

dropChildren :: Int -> IOSArrow XmlTree XmlTree 
dropChildren i = processChildrenList (drop i)

takeChildren :: Int -> IOSArrow XmlTree XmlTree 
takeChildren i = processChildrenList (take i)

-- | Modifies a tree by running a function on the list of children
processChildrenList :: ([XmlTree] -> [XmlTree]) -> IOSArrow XmlTree XmlTree
processChildrenList f = traceMsg 1 "processChildrenList"
               >>>
               getNode -- get tree root
               &&&
               (listA (getChildren >>. f)) -- get tree children, throw away 2
               >>> arr2 T.mkTree -- construct new tree


-------------------------------------------------------------------------------
-- Misc utilities

parseCredentials :: String -> IO Credentials
parseCredentials path = do
    h <- openFile path ReadMode
    user <- hGetLine h
    pass <- hGetLine h
    hClose h
    return (user,pass)

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _   = False

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

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

