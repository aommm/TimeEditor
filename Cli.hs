
module Main (
	main
  ) where

import Api
import Api.Util
import Api.MyBookings

-- Networking
import Network.Wreq
import qualified Network.Wreq.Session as S

-------------------------------------------------------------------------------
-- Main program

main :: IO ()
main = S.withSession $ \sess -> do
    -- timeFun
    creds <- parseCredentials "credentials"
    login sess creds
    bookings <- getBookings sess
    print bookings
