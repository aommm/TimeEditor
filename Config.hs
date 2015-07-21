
module Main (
	main
  ) where

import Api
import Api.Util
import Api.MyBookings
import Api.MakeBooking

import qualified Backend as B

-- Networking
import Network.Wreq
import qualified Network.Wreq.Session as S

-- Misc
import Control.Monad.Reader
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import Data.Time
import Data.Char (toLower)
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)

import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Types

data Data = Data {
  rooms    :: [Room],
  purposes :: [Purpose]
} deriving (Show, Read)

emptyData :: Data
emptyData = Data {rooms = [], purposes = []}

type Program = ReaderT Data IO

-------------------------------------------------------------------------------
-- Main program

-- TODO: Add functionality to menu options
-- 

-- TODO: add command line arguments
--       --config, --clear, --add, --remove --list


path, dataPath :: FilePath
path = "bookings.dat"
dataPath = "data.dat"
credentialsPath = "credentials"

main :: IO ()
main = do
  myData <- initData
  runReaderT mainMenu myData

mainMenu :: Program ()
mainMenu = do
  putStrLn' "\n********************"
  putStrLn' "TimeEditor - configuration"
  putStrLn' "********************"
  putStrLn' "\n1. Show bookings"
  putStrLn' "2. Add new booking"
  putStrLn' "E. Exit"
  mainMenu'

mainMenu' = do
  putStr' "Enter what you want to do: "
  opt <- getLine'
  case toLowerCase opt of
    "1" -> do
      bookingsMenu
      mainMenu
    "2" -> do
      addBookingMenu
      mainMenu
    "e" ->
      return ()
    otherwise ->
      askAgain mainMenu'

bookingsMenu = do
  rbs <- liftIO $ B.getRecurringBookings path
  putStrLn' "\nYour recurring bookings:"
  putStrLn' "------------------------"
  let rbStrings = map (\(rb,i) -> show i ++ ". " ++ rbToString rb) (rbs `zip` [0..])
  mapM_ putStrLn' rbStrings
  bookingsMenu' rbs

bookingsMenu' rbs = do
  putStr' "\nEnter any number to view that booking, or E to go back: "
  opt <- getLine'
  case toLowerCase opt of
    "e" ->
      return ()
    inp -> do
      case readMaybe inp of 
        Nothing ->
          askAgain $ bookingsMenu' rbs
        Just i -> 
          if i < 0 || i >= (length rbs)
          then do
            putStrLn' "That booking doesn't exist. Try again."
            bookingsMenu' rbs
          else do
            bookingMenu rbs i
            bookingsMenu

bookingMenu rbs i = do
    putStrLn' "\nShowing detailed info"
    putStrLn' "------------------------"
    let rb = rbs !! i
    putStrLn' $ rbToString rb
    putStrLn' "1. Delete booking"
    putStrLn' "2. Modify booking"
    putStrLn' "E. Go back"
    bookingMenu' rbs i

bookingMenu' rbs i = do
    putStr'   "\nwhat do you want to do? "
    opt <- getLine'
    case toLowerCase opt of
      "1" -> do
        liftIO $ B.removeRecurringBooking path i
        putStrLn' "Booking removed."
      "2" -> do
        modifyBookingMenu rbs i
      "e" ->
        return ()
      otherwise -> askAgain $ bookingMenu' rbs i

-- TODO
modifyBookingMenu rbs i = do
  putStrLn' "\nModifying booking"
  putStrLn' "-------------------"


-------------------------------------------------------------------------------
-- Add bookings

-- TODO: initiate a session somehow

-- TODO implement
addBookingMenu = do
  putStrLn' "\nAdding booking"
  putStrLn' "--------------"
  data' <- ask
  let allRooms    = rooms data'
      allPurposes = purposes data'
  -- TODO: allow user to enter several rooms/purposes?
  putStrLn' $ "available rooms:"++show allRooms
  putStrLn' $ "available purposes:"++show allPurposes
  
  -- TODO: create RB here
  -- let b = Booking {startTime = t1, endTime = t2, room = head allRooms,
  --                  purpose = head allPurposes, publicComment="I am an ordinary citizen; stroll",
  --                  privateComment="I am a robot" }
  



-------------------------------------------------------------------------------
-- Init

-- | Gets the cached state from file,
--   or downloads it if it doesn't exist
initData :: IO Data
initData = do
  putStrLn "initData"
  mData <- getCachedData -- See if cached data exists
  case mData of
    Just data' -> return data' -- If so, return cached data
    Nothing    -> do           -- If not, download data and put in cache
      data' <- getOnlineData
      saveCachedData data'
      return data'

-- | Gets the cached data from file
-- TODO: fix so that "data.dat" is created when it does not exist
getCachedData :: IO (Maybe Data)
getCachedData = do
  putStrLn "getCachedData"
  handle <- openFile dataPath ReadMode
  s <- hGetContents handle
  let bs  = if null s
            then Nothing
            else Just ((read s) :: Data)
  hClose handle
  return bs

-- | Saves data to file
-- TODO: fix so that "data.dat" is created when it does not exist
saveCachedData :: Data -> IO ()
saveCachedData data' = do
  putStrLn "saveCachedData"
  handle <- openFile dataPath WriteMode 
  hPutStr handle (show data')
  hClose handle

-- Calculates latest possible times to make a booking at
getLatestTimes :: S.Session -> IO (Time,Time)
getLatestTimes sess = do
  times <- getAvailableTimes sess
  let (_,t2)  = fromJust times
      oneHour = 3600
      t1 = addUTCTime (-oneHour) t2
  return (t1,t2)

-- | Downloads data from the net and returns it
getOnlineData :: IO Data
getOnlineData = S.withSession $ \sess -> do
  putStrLn "getOnlineData"
  creds <- parseCredentials credentialsPath
  login sess creds
  t <- getLatestTimes sess
  newRooms    <- getAvailableRooms sess t
  newPurposes <- getAvailablePurposes sess
  return Data {
    rooms    = newRooms,
    purposes = newPurposes
  }

-------------------------------------------------------------------------------
-- Program commands

putStr' :: String -> Program ()
putStr' s = liftIO $ do
  putStr s
  hFlush stdout

putStrLn' :: String -> Program ()
putStrLn' s = liftIO $ putStrLn s

getLine' :: Program String
getLine' = liftIO getLine

askAgain :: Program a -> Program a
askAgain p = do
  putStrLn' "I didn't quite grasp that. Please try again."
  p

-------------------------------------------------------------------------------
-- Util

rbToString :: B.RecurringBooking -> String
rbToString rb = B.rName rb++" (next time "++show (B.rStartTime rb)
                    ++", every "++ show (B.everyXWeeks rb)  ++ " weeks)"

toLowerCase :: String -> String
toLowerCase s = map toLower s
