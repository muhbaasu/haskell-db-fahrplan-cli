module Main where

import           Control.Monad              (liftM2)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Either                (either)
import           Data.List                  (sort, zipWith)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, unpack, pack)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.LocalTime        (LocalTime (..), localDay, localTimeOfDay, utcToLocalTime, getTimeZone)
import           Options.Applicative
import           Servant.Client             (ServantError)

import           DeutscheBahn.Schedule.Cli
import           Web.DeutscheBahn.API.Schedule.API
import           Web.DeutscheBahn.API.Schedule.Data

data StationBoard = StationBoard
  { _stationBoardStop :: StopLocation
  , _stationBoardMsgs :: [String]
  } deriving (Show, Eq)

instance Monoid StationBoard where
  mempty = StationBoard (StopLocation (StopId $ pack "0") (pack "") (Coordinate 0.0 0.0)) []
  a `mappend` b = StationBoard stop msgs
    where stop = _stationBoardStop a
          msgs = _stationBoardMsgs a <> _stationBoardMsgs b

instance Ord StopLocation where
  compare a b = _stopLocationId a `compare` _stopLocationId b
  (<=) a b = _stopLocationId a <= _stopLocationId b

instance Ord StationBoard where
  compare a b = _stationBoardStop a `compare` _stationBoardStop b
  (<=) a b = _stationBoardStop a <= _stationBoardStop b

main :: IO ()
main = do
  params   <- execParser (info (helper <*> bahncliP) programInfo)
  now      <- localNow
  boards   <- runEitherT $ queryBoards params (selectTime params now)
  putStrLn $ either show (unlines . map formatBoard) boards
  return ()

-- | Select parameter time or current local time
selectTime :: BahnCliParam -> LocalTime -> LocalTime
selectTime params now = LocalTime selectDay selectTime
  where selectDay  = fromMaybe (localDay now) (day params)
        selectTime = fromMaybe (localTimeOfDay now) (time params)

queryBoards :: BahnCliParam -> LocalTime -> EitherT ServantError IO [StationBoard]
queryBoards p localNow = do
  stops'     <- locationName Nothing (authKey p) (stopN p)
  departures <- mapM queryDepartures stops'
  arrivals   <- mapM queryArrivals stops'
  return $ sortAndZip departures arrivals
  where queryDepartures s = StationBoard s . map formatDeparture <$> departureBoard Nothing (authKey p) (_stopLocationId s) localNow
        queryArrivals s   = StationBoard s . map formatArrival <$> arrivalBoard Nothing (authKey p) (_stopLocationId s) localNow

sortAndZip :: [StationBoard] -> [StationBoard] -> [StationBoard]
sortAndZip a b = zipWith (<>) (sort a) (sort b)

localNow :: IO LocalTime
localNow = do
  now      <- getCurrentTime
  timeZone <- getTimeZone now
  return $ utcToLocalTime timeZone now

formatBoard :: StationBoard -> String
formatBoard board =
     let stop = unpack $ _stopLocationName $ _stationBoardStop board
         msgs = unlines $ _stationBoardMsgs board
     in "Connections for stop: " <> stop <> "\n" <> msgs

formatDeparture :: Departure -> String
formatDeparture d =
     "departs "
  <> show (_departureDateTime d)                       <> "\t"
  <> show (_departureName d)                           <> "\t"
  <> "track: " <> maybe "?" unpack (_departureTrack d) <> "\t"
  <> "with: "  <> show (_departureTransportType d)     <> "\t"
  <> "to: "    <> unpack (_departureDirection d)       <> "\n"

formatArrival :: Arrival -> String
formatArrival d =
     "arrives "
  <> show (_arrivalDateTime d)                       <> "\t"
  <> show (_arrivalName d)                           <> "\t"
  <> "track: " <> maybe "?" unpack (_arrivalTrack d) <> "\t"
  <> "with: "  <> show (_arrivalTransportType d)     <> "\t"
  <> "from: "    <> unpack (_arrivalOrigin d)       <> "\n"

