{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module DeutscheBahn.Schedule.Cli where

import           Control.Monad            (join)
import           Data.Monoid              ((<>))
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
  , boardType    :: BoardType
  , lang         :: ApiLanguage
  , stopN        :: Text
  } deriving (Show)

bahncliP :: Parser BahnCliParam
bahncliP = BahnCliParam
  <$> authKeyP
  <*> timeP
  <*> ((fromMaybe BoardAll) <$> boardTypeP)
  <*> ((fromMaybe English) <$> languageP)
  <*> (pack <$> (argument str (metavar "LOCATION")))

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
   <> help "API Authentication Key provided by Deutsche Bahn" )

timeP :: Parser (Maybe TimeOfDay)
timeP = join <$> (fmap . fmap) ptime (optional $ strOption
    ( long "time"
   <> short 't'
   <> metavar "TIME"
   <> help "Time of day e.g. 20:22 or 05:20" ))

ptime :: String -> Maybe TimeOfDay
ptime = parseTimeM True defaultTimeLocale "%H:%M"

boardTypeP :: Parser (Maybe BoardType)
boardTypeP =
  optional $ option auto
    ( long "board"
   <> short 'b'
   <> metavar "BOARD"
   <> help "`arrival` || `departure` || `all`" )

