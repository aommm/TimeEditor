{-# LANGUAGE OverloadedStrings #-}

module Api (
	-- Types
  	Booking (..),
  	Time,
    Room,
  	Purpose,
  	Credentials,
  	Username,
  	Password,
    PrivateComment,
    PublicComment,
  	-- Operations
  	login
  ) where

-- Networking
import Network.Wreq
import qualified Network.Wreq.Session as S

import Data.Time
import System.Locale -- locale needed for time

-------------------------------------------------------------------------------
-- Types

-- A room booking
data Booking = Booking {
  startTime :: Time,
  endTime   :: Time,
  room      :: Room,
  purpose   :: Purpose,
  privateComment :: PrivateComment,
  publicComment  :: PublicComment
} deriving (Eq, Show)

type Time    = UTCTime

type Id   = String
type Name = String
type Room    = (Id,Name)
type Purpose = (Id,Name)

type PrivateComment = String
type PublicComment  = String

-- Login credentials
type Credentials = (Username,Password)
type Username    = String
type Password    = String

-------------------------------------------------------------------------------
-- Operations

login :: S.Session -> Credentials -> IO ()
login sess (user,pass) = do
    let loginUrl = "https://se.timeedit.net/web/chalmers/db1/b1/r.html?h=t&sid=1002&id=-1"
    let loginData = ["authServer" := ("student" :: String), "username" := user, "password" := pass]
    r <- S.post sess loginUrl loginData
    return ()

