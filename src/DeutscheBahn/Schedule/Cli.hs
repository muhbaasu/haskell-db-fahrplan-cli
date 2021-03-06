{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module DeutscheBahn.Schedule.Cli where

import           Control.Monad            (join)
import           Data.Monoid              ((<>))
import           Data.Time.Calendar       (Day (..))
import           Data.Time.LocalTime      (LocalTime (..), TimeOfDay (..))
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)
import           Options.Applicative
import           Data.Text                (Text, pack)
import           Data.Maybe               (fromMaybe)

import Web.DeutscheBahn.API.Schedule.API
import Web.DeutscheBahn.API.Schedule.Data

data BoardType =
  -- | Arrivals only
  BoardArrival |
  -- | Departures only
  BoardDeparture |
  -- | Arrivals and Departures
  BoardAll
  deriving Show

instance Read BoardType where
  readsPrec _ "arrival"   = [(BoardArrival, "")]
  readsPrec _ "departure" = [(BoardDeparture, "")]
  readsPrec _ "all"       = [(BoardAll, "")]
  readsPrec _ _           = [(BoardAll, "")]

instance Read ApiLanguage where
  readsPrec _ "de" = [(German, "")]
  readsPrec _ "en" = [(English, "")]
  readsPrec _ _    = [(English, "")]

data BahnCliParam = BahnCliParam
  {
    authKey      :: AuthKey
  , time         :: Maybe TimeOfDay
  , day          :: Maybe Day
  , boardType    :: BoardType
  , lang         :: ApiLanguage
  , stopN        :: Text
  } deriving (Show)

bahncliP :: Parser BahnCliParam
bahncliP = BahnCliParam
  <$> authKeyP
  <*> timeP
  <*> dayP
  <*> ((fromMaybe BoardAll) <$> boardTypeP)
  <*> ((fromMaybe English) <$> languageP)
  <*> (pack <$> (argument str (metavar "LOCATION")))

programInfo :: InfoMod a
programInfo = ( fullDesc
                   <> progDesc "DeutscheBahn CLI v0.1"
                   <> header "DeutscheBahn CLI - Retrieve schedules via the command line" )

languageP :: Parser (Maybe ApiLanguage)
languageP = optional $ option auto
    ( long "language"
   <> short 'l'
   <> metavar "LANG"
   <> help "Response language `de` or `en`, default is `en`" )

authKeyP :: Parser AuthKey
authKeyP = AuthKey
  <$> pack
  <$> strOption
    ( long "key"
   <> short 'k'
   <> metavar "AUTHKEY"
   <> help "API Authentication Key provided by Deutsche Bahn." )

timeP :: Parser (Maybe TimeOfDay)
timeP = join <$> (fmap . fmap) ptime (optional $ strOption
    ( long "time"
   <> short 't'
   <> metavar "TIME"
   <> help "Time of day e.g. 20:22 or 05:20. Defaults to now." ))

dayP :: Parser (Maybe Day)
dayP = join <$> (fmap . fmap) pday (optional $ strOption
    ( long "day"
   <> short 'd'
   <> metavar "Day"
   <> help "Day e.g. 30.12.2016. Defaults to today." ))

ptime :: String -> Maybe TimeOfDay
ptime = parseTimeM True defaultTimeLocale "%H:%M"

pday :: String -> Maybe Day
pday = parseTimeM True defaultTimeLocale "%d.%m.%Y"

boardTypeP :: Parser (Maybe BoardType)
boardTypeP =
  optional $ option auto
    ( long "board"
   <> short 'b'
   <> metavar "BOARD"
   <> help "`arrival` or `departure` or `all`" )

