
module Main (
	main
  ) where

import Api
import Api.Util
import Api.MyBookings
import Api.MakeBooking

import Backend

-- Networking
import Network.Wreq
import qualified Network.Wreq.Session as S

-- Misc
import Data.Time
import Data.Maybe (fromJust)


-------------------------------------------------------------------------------
-- Main program

main :: IO ()
main = makeBookingTest



backendTest :: IO ()
backendTest = do
	let path = "bookings.dat"
	putStrLn "adding booking... "
	addRecurringBooking path simpleRecBooking
	putStrLn "... done!"

-----------------

myBookingsTest :: IO ()
myBookingsTest = S.withSession $ \sess -> do
    creds <- parseCredentials "credentials"
    login sess creds
    bookings <- getBookings sess
    print bookings

makeBookingTest :: IO ()
makeBookingTest = S.withSession $ \sess -> do
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
    -- putStrLn $ snd $ purposes !! 0
    let b = Booking {startTime = t1, endTime = t2, room = head rooms,
                     purpose = head purposes, publicComment="I am an ordinary citizen; stroll",
                     privateComment="I am a robot" }
    result <- makeBooking sess b
    print result
    return ()


