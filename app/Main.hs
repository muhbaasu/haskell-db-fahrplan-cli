module Main where

import           Control.Monad (join)
import           Control.Monad.Trans.Either (runEitherT)
import           Data.Either        (either)
import           Data.Maybe               (fromMaybe)
import           Data.Text                  (Text, unpack)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.LocalTime        (utcToLocalTime, getTimeZone)
import           Options.Applicative

import           DeutscheBahn.Schedule.Cli
import           Web.DeutscheBahn.API.Schedule.API
import           Web.DeutscheBahn.API.Schedule.Data

main :: IO ()
main = do
  p     <- execParser (info bahncliP idm)
  now   <- getCurrentTime
  timeZone <- getTimeZone now
  localNow <- return $ utcToLocalTime timeZone now
  board <- runEitherT $ do
    stopId'    <- (_stopLocationId . head) <$> locationName Nothing (authKey p) (stopN p)
    departureBoard Nothing (authKey p) stopId' localNow
  putStrLn $ either (const "Failed to retrieve departures") (\b -> unlines $ map formatDeparture b) board
  return ()

formatDeparture :: Departure -> String
formatDeparture d =
     (show $ _departureDateTime d) <> "\t"
  <> (show $ _departureName d)     <> "\t"
  <> "track: " <> (fromMaybe "?" (fmap unpack $ _departureTrack d)) <> "\t"
  <> "with: " <> (show $ _departureTransportType d) <> "\t"
  <> "to: " <> (unpack $ _departureDirection d)  <>  "\n"

