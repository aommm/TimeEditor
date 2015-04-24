
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

-- TODO: add command line arguments
--       --config, --clear, --add, --remove --list

main :: IO ()
main = do
  putStrLn "********************"
  putStrLn "TimeEditor - configuration of recurring bookings"
  putStrLn "********************"
  let path = "bookings.dat"
  rbs <- getRecurringBookings path
  putStrLn "Your recurring bookings:"
  mapM_ (putStrLn.rbToString) (rbs `zip` [0..])
  putStrLn "What do you want to do?"
  putStrLn "(Enter 'add' to add new, or one of the numbers to modify existing)"


rbToString :: (RecurringBooking,Int) -> String
rbToString (rb,i) = show i++". "++rName rb++" (next time "++show (rStartTime rb)
                    ++", every "++ show (everyXWeeks rb)  ++ " weeks)"