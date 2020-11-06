{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse
    (
      parseSummoner,
      Summoner(..),
      parseMatchList,
      MatchList(..),
      MatchInfo(..),
      parseMatch,
      Match(..),
      Team(..),
      Participant(..),
      ParticipantStats(..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

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

-- /parseSummoner is a function which takes a Lazy Bytestring, and converts it
-- to the Haskell data type, Summoner, which represent a League of Legends player's account.
parseSummoner :: L8.ByteString -> Either String Summoner
parseSummoner json = eitherDecode json :: Either String Summoner


-- Create MatchList record types and parser 
-- MatchList record type, contains an index of previous matches (for a single player):
data MatchList = MatchList {
    ml_startIndex :: Int,
    ml_endIndex :: Int,
    ml_matches :: [MatchInfo]
} deriving (Show, Generic)

-- MatchInfo record type, for individual match reference information:
data MatchInfo = MatchInfo {
    mi_gameId :: Int,
    mi_role :: String,
    mi_season :: Int,
    mi_platformId :: String,
    mi_champion :: Int,
    mi_queue :: Int,
    mi_lane :: String,
    mi_timestamp :: Int
} deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''MatchInfo)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''MatchList)

-- /parseMatchList will take a Lazy Bytestring, and parse it in to the Haskell
-- datatype for MatchList (containing instances of MatchInfo as well).
parseMatchList :: L8.ByteString -> Either String MatchList
parseMatchList json = eitherDecode json :: Either String MatchList


-- Create Match record types and parser
data Match = Match {
    m_gameId :: Int,
    m_gameCreation :: Int,
    m_gameDuration :: Int,
    m_gameMode :: String,
    m_gameType :: String,
    m_teams :: [Team],
    m_participants :: [Participant],
    m_participantIdentities :: [ParticipantIdentity]
} deriving (Show, Generic)

-- This type denotes a team belonging to a particular game
data Team = Team {
    t_teamId :: Int,
    t_win :: String,
    t_firstBlood :: Bool,
    t_firstTower :: Bool,
    t_towerKills :: Int,
    t_inhibitorKills :: Int,
    t_baronKills :: Int,
    t_dragonKills :: Int,
    t_riftHeraldKills :: Int
} deriving (Show, Generic)

-- This type denotes a instance of a player IN a match
data Participant = Participant {
    p_participantId :: Int,
    p_teamId :: Int,
    p_championId :: Int,
    p_stats :: ParticipantStats
} deriving (Show, Generic)

-- This type denotes a players stats for a single given match
data ParticipantStats = ParticipantStats {
    ps_participantId :: Int,
    ps_win :: Bool,
    ps_kills :: Int,
    ps_deaths :: Int,
    ps_assists :: Int,
    ps_largestKillingSpree :: Int,
    ps_largestMultiKill :: Int,
    ps_totalDamageDealt :: Int,
    ps_totalDamageDealtToChampions :: Int,
    ps_totalDamageTaken :: Int,
    ps_goldEarned :: Int,
    ps_goldSpent :: Int,
    ps_totalMinionsKilled :: Int
} deriving (Show, Generic)

data ParticipantIdentity = ParticipantIdentity {
    pi_participantId :: Int,
    pi_accountId :: String,
    pi_summonerName :: String,
    pi_summonerId :: String,
    pi_currentAccountId :: String
} deriving (Show, Generic)

instance FromJSON ParticipantIdentity where
    parseJSON = withObject "participant" $ \o -> do
        pi_participantId <- o .: "participantId"
        player <- o .: "player"
        pi_accountId <- player .: "accountId"
        pi_summonerName <- player .: "summonerName"
        pi_summonerId <- player .: "summonerId"
        pi_currentAccountId <- player .: "currentAccountId"
        return ParticipantIdentity{..}

instance ToJSON ParticipantIdentity where
    toJSON p = object [
        "participantIdentity" .= pi_participantId p,
        "player" .= object [
            "accountId" .= pi_accountId p,
            "summonerName" .= pi_summonerName p,
            "summonerId" .= pi_summonerId p,
            "currentAccountId" .= pi_currentAccountId p
            ]
        ]

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''ParticipantStats)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Participant)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Team)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Match)

-- Define the parser for a match:
parseMatch :: L8.ByteString -> Either String Match
parseMatch json = eitherDecode json :: Either String Match