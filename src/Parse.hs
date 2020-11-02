{-# LANGUAGE DeriveGeneric #-}

module Parse
    ( parseLeaderboard,
      Leaderboard(entries)
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

-- Create record types for leaderboard:
-- Overall leaderboard type
data Leaderboard = Leaderboard {
    tier :: String,
    leagueId :: String,
    queue :: String,
    name :: String,
    entries :: [LeagueEntry]
} deriving (Show, Generic)

instance FromJSON Leaderboard
instance ToJSON Leaderboard

-- Individual player entries on this leaderboard
data LeagueEntry = LeagueEntry {
    summonerId :: String,
    summonerName :: String,
    leaguePoints :: Int,
    rank :: String,
    wins :: Int,
    losses :: Int,
    veteran :: Bool,
    inactive :: Bool,
    freshBlood :: Bool,
    hotStreak :: Bool
} deriving (Show, Generic)

instance FromJSON LeagueEntry
instance ToJSON LeagueEntry

-- Create parser for handling leaderboard JSON data.
parseLeaderboard :: L8.ByteString -> Either String Leaderboard
parseLeaderboard json = eitherDecode json :: Either String Leaderboard