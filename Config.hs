
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
import Data.Time
import Data.Char (toLower)
import Data.Maybe (fromJust)

import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Main program

-- TODO: Add functionality to menu options
-- 

-- TODO: add command line arguments
--       --config, --clear, --add, --remove --list

path :: String
path = "bookings.dat"

main :: IO ()
main = do
  mainMenu

mainMenu :: IO ()
mainMenu = do
  putStrLn "\n********************"
  putStrLn "TimeEditor - configuration"
  putStrLn "********************"
  putStrLn "\n1. Show bookings"
  putStrLn "2. Add new booking"
  putStrLn "E. Exit"
  mainMenu'

mainMenu' = do
  putStr "Enter what you want to do: "
  opt <- getLine
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
  rbs <- B.getRecurringBookings path
  putStrLn "\nYour recurring bookings:"
  putStrLn "------------------------"
  let rbStrings = map (\(rb,i) -> show i ++ ". " ++ rbToString rb) (rbs `zip` [0..])
  mapM_ putStrLn rbStrings
  bookingsMenu' rbs

bookingsMenu' rbs = do
  putStr "\nEnter any number to view that booking, or E to go back: "
  opt <- getLine
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
            putStrLn "That booking doesn't exist. Try again."
            bookingsMenu' rbs
          else do
            bookingMenu rbs i
            bookingsMenu

bookingMenu rbs i = do
    putStrLn "\nShowing detailed info"
    putStrLn "------------------------"
    let rb = rbs !! i
    putStrLn $ rbToString rb
    putStrLn "1. Delete booking"
    putStrLn "2. Modify booking"
    putStrLn "E. Go back"
    bookingMenu' rbs i

bookingMenu' rbs i = do
    putStr   "\nwhat do you want to do? "
    opt <- getLine
    case toLowerCase opt of
      "1" -> do
        B.removeRecurringBooking path i
        putStrLn "Booking removed."
      "2" -> do
        modifyBookingMenu rbs i
      "e" ->
        return ()
      otherwise -> askAgain $ bookingMenu' rbs i

-- TODO
modifyBookingMenu rbs i = do
  putStrLn "\nModifying booking"
  putStrLn "-------------------"

-- TODO
addBookingMenu = do
  putStrLn "\nAdding booking"
  putStrLn "-----------------"

-------------------------------------------------------------------------------
-- Util

askAgain :: IO a -> IO a
askAgain p = do
  putStrLn "I didn't quite grasp that. Please try again."
  p

rbToString :: B.RecurringBooking -> String
rbToString rb = B.rName rb++" (next time "++show (B.rStartTime rb)
                    ++", every "++ show (B.everyXWeeks rb)  ++ " weeks)"

toLowerCase :: String -> String
toLowerCase s = map toLower s
