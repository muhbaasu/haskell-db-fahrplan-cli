module Main where

import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Either                (either)
import           Data.Text                  (Text, unpack)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.LocalTime        (LocalTime, utcToLocalTime, getTimeZone)
import           Options.Applicative
import           Servant.Client             (ServantError)

import           DeutscheBahn.Schedule.Cli
import           Web.DeutscheBahn.API.Schedule.API
import           Web.DeutscheBahn.API.Schedule.Data

main :: IO ()
main = do
  params   <- execParser (info bahncliP idm)
  now      <- localNow
  boards   <- runEitherT $ queryBoards params now
  putStrLn $ either show (unlines . map format) boards
  return ()
  where format (loc, deps) = formatBoard loc deps

queryBoards :: BahnCliParam -> LocalTime -> EitherT ServantError IO [(StopLocation, [Departure])]
queryBoards p localNow = do
  stops'  <- locationName Nothing (authKey p) (stopN p)
  mapM query stops'
  where query d = (,) d <$> departureBoard Nothing (authKey p) (_stopLocationId d) localNow

localNow :: IO LocalTime
localNow = do
  now      <- getCurrentTime
  timeZone <- getTimeZone now
  return $ utcToLocalTime timeZone now

formatBoard :: StopLocation -> [Departure] -> String
formatBoard stop departures =
     "Connections for stop: " <> unpack (_stopLocationName stop) <> "\n"
  <> unlines (map formatDeparture departures)

formatDeparture :: Departure -> String
formatDeparture d =
     show (_departureDateTime d)                       <> "\t"
  <> show (_departureName d)                           <> "\t"
  <> "track: " <> maybe "?" unpack (_departureTrack d) <> "\t"
  <> "with: "  <> show (_departureTransportType d)     <> "\t"
  <> "to: "    <> unpack (_departureDirection d)       <> "\n"

