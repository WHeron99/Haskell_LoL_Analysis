{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Parse
    ( parseLeaderboard,
      parseSummoner,
      Leaderboard(..),
      LeagueEntry(..),
      Summoner(..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

-- Create record types for leaderboard and its parser
-- Individual player entries on this leaderboard
data LeagueEntry = LeagueEntry {
    le_summonerId :: String,
    le_summonerName :: String,
    le_leaguePoints :: Int,
    le_rank :: String,
    le_wins :: Int,
    le_losses :: Int,
    le_veteran :: Bool,
    le_inactive :: Bool,
    le_freshBlood :: Bool,
    le_hotStreak :: Bool
} deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''LeagueEntry)

-- Overall leaderboard type
data Leaderboard = Leaderboard {
    lb_tier :: String,
    lb_leagueId :: String,
    lb_queue :: String,
    lb_name :: String,
    lb_entries :: [LeagueEntry]
} deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Leaderboard)

-- Create parser for handling leaderboard JSON data.
parseLeaderboard :: L8.ByteString -> Either String Leaderboard
parseLeaderboard json = eitherDecode json :: Either String Leaderboard


-- Create record type for Summoner information and parser 
-- Summoner record type:
data Summoner = Summoner {
    s_id :: String,
    s_accountId :: String,
    s_puuid :: String,
    s_name :: String,
    s_profileIconId :: Int,
    s_revisionDate :: Int,
    s_summonerLevel :: Int
} deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Summoner)

parseSummoner :: L8.ByteString -> Either String Summoner
parseSummoner json = eitherDecode json :: Either String Summoner


-- Create MatchList record type and parser 
-- TODO

-- Create Match record type and parser
-- TODO