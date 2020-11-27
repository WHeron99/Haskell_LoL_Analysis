{-|
Module      : Parse
Stability   : experimental
Portability : POSIX

This module is responsible for converting JSON ByteStrings to Haskell Datatypes, which are also defined in this
    module. This modules exports include the defined datatypes, and the parse functions. The Participant and
    ParticipantIdentity types each have custom designed parsers. The remaining types automatically derive Generic
    but make use of some language extensions to append name extensions to their fields to prevent ambiguity errors,
    such as in the case of the id name, which conflicts directly with Prelude.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse
    (
        -- * Summoner Parsing
        parseSummoner,
        Summoner(..),
        -- * MatchList Parsing
        parseMatchList,
        MatchList(..),
        MatchInfo(..),
        -- * Match Parsing
        parseMatch,
        Match(..),
        Team(..),
        Participant(..),
        ParticipantIdentity(..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics


{- |
    'Summoner' is a data type for holding a League of Legends player's account information.
-}
data Summoner = Summoner {
    s_id :: String,         -- ^ The accounts summonerId
    s_accountId :: String,  -- ^ The accounts accountId
    s_puuid :: String,      -- ^ The accounts puuid, which is persistent across all game servers
    s_name :: String,       -- ^ The accounts current visble player name
    s_profileIconId :: Int, -- ^ The accounts current active profile icon, given as an ID for the icon
    s_revisionDate :: Int,  -- ^ The last time this accounts record was updated
    s_summonerLevel :: Int  -- ^ The accounts current level, derived from the amount of games they have played/experience they have earned
} deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Summoner)

{- |
    'parseSummoner' is a function which takes a Lazy 'ByteString', and converts it
        to the Haskell data type, 'Summoner', which represent a League of Legends player's account.
-}
parseSummoner :: L8.ByteString -> Either String Summoner
parseSummoner json = eitherDecode json :: Either String Summoner


{- | 
    'MatchList' record type, contains information regarding the information retrieved from the matchlist
        endpoint, including 'MatchInfo' for the returned matches
-}
data MatchList = MatchList {
    ml_startIndex :: Int,           -- ^ The index of the first match in the list (in context of all avaiable matches for this player)
    ml_endIndex :: Int,             -- ^ The index of the last match in the list
    ml_matches :: [MatchInfo]       -- ^ A list of brief summary information for each match
} deriving (Show, Generic)

{- |
    The 'MatchInfo' record type contains brief synopsis information regarding games, though not in as much
        detail as 'Match'.
-}
data MatchInfo = MatchInfo {
    mi_gameId :: Int,               -- ^ The unique ID for the match
    mi_role :: String,              -- ^ The (predicted) role the given player played this match
    mi_season :: Int,               -- ^ The ranked season the match was played in (increments each year)
    mi_platformId :: String,        -- ^ The sever region this match was played on
    mi_champion :: Int,             -- ^ The unique ID of the champion the player played
    mi_queue :: Int,                -- ^ What type of queue (matchmaking style) the player queued up with
    mi_lane :: String,              -- ^ The (predicted) lane the given player played in this match
    mi_timestamp :: Int             -- ^ The time the match was created, given as time since epoch (milliseconds)
} deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''MatchInfo)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''MatchList)

{- |
    'parseMatchList' will take a Lazy 'ByteString', and parse it in to the Haskell
        datatype for 'MatchList' (containing instances of ['MatchInfo'] as well).
-}
parseMatchList :: L8.ByteString -> Either String MatchList
parseMatchList json = eitherDecode json :: Either String MatchList


{- |
    'Match' contains greater detail regarding a single match compared to that of 
        'MatchInfo'. 
-}
data Match = Match {
    m_gameId :: Int,                                    -- ^ The games unique ID
    m_gameCreation :: Int,                              -- ^ The time the match was created, given as time since epoch (milliseconds)
    m_gameDuration :: Int,                              -- ^ The length of the game (in seconds)
    m_gameMode :: String,                               -- ^ The game mode that was played
    m_gameType :: String,                               -- ^ The type of match that was played
    m_teams :: [Team],                                  -- ^ A list containing both 'Team's from this match
    m_participants :: [Participant],                    -- ^ A list of the 10 'Participant's from this match
    m_participantIdentities :: [ParticipantIdentity]    -- ^ A list of the 10 'Participant's 'ParticipantIdentity's from this match - which detail account information for each.
} deriving (Show, Generic)

{- |
    'Team' contains information regarding each team from a given match
-}
data Team = Team {
    t_teamId :: Int,                -- ^ The teams unique ID (100 or 200, for Blue or Red respectively)
    t_win :: String,                -- ^ Did the given team win? (Given as a string)
    t_firstBlood :: Bool,           -- ^ Did this team get the first kill of the match?
    t_firstTower :: Bool,           -- ^ Did this team kill the first tower of the match?
    t_towerKills :: Int,            -- ^ Total number of turrets killed by this team
    t_inhibitorKills :: Int,        -- ^ Total number of inhibitors killed by this team
    t_baronKills :: Int,            -- ^ Total number of times this team killed Baron Nashor (A map objective/boss fight)
    t_dragonKills :: Int,           -- ^ Total number of times this team killed the Dragon (A map objective/boss)
    t_riftHeraldKills :: Int        -- ^ Total number of times this team killed the Rift Herald (A map objective/boss)
} deriving (Show, Generic)

{- |
    'Participant' contains information regarding to an individual players performance in a given 'Match'
-}
data Participant = Participant {
    p_participantId :: Int,                 -- ^ The unique identifier for this player in this 'Match'
    p_teamId :: Int,                        -- ^ The 'Team' that this player was a part of for this 'Match'
    p_championId :: Int,                    -- ^ The unique ID of the character/champion that this player played this match
    p_win :: Bool,                          -- ^ Did the given player's 'Team' win this match?
    p_kills :: Int,                         -- ^ How many enemy players did this player kill?
    p_deaths :: Int,                        -- ^ How many times did this player die in this match?
    p_assists :: Int,                       -- ^ How many times did this player assist in the killing of another enemy player?
    p_largestKillingSpree :: Int,           -- ^ Maximum number of kills the player achieved without a death
    p_largestMultiKill :: Int,              -- ^ Maximum number of kills the player got in quick succession
    p_totalDamageDealt :: Int,              -- ^ Total amount of damage dealt by the player in this match
    p_totalDamageDealtToChampions :: Int,   -- ^ Total amount of damage dealt by the player, only to enemy players
    p_totalDamageTaken :: Int,              -- ^ Total amount of damage this player took from all sources
    p_goldEarned :: Int,                    -- ^ Total amount of gold that the player earned in this match
    p_goldSpent :: Int,                     -- ^ Total amount of gold that the player spent on items in this match
    p_totalMinionsKilled :: Int             -- ^ Total number of minions (non-playable target creatures) this player killed in this match
} deriving (Show, Generic)

{- |
    'ParticipantIdentity' contains information regarding to a individual players account information for a given 'Match'
-}
data ParticipantIdentity = ParticipantIdentity {
    pi_participantId :: Int,                -- ^ The unique identifier for this player in this 'Match'
    pi_accountId :: String,                 -- ^ The accountId for this given player at the time of the match
    pi_summonerName :: String,              -- ^ The players name at the time of the match
    pi_summonerId :: String,                -- ^ The summonerId for the given player
    pi_currentAccountId :: String           -- ^ The accountId for this given player at this current time
} deriving (Show, Generic)

instance FromJSON ParticipantIdentity where
    parseJSON = withObject "participant" $ \o -> do
        pi_participantId <- o .: "participantId"
        player <- o .: "player"                         -- Get player sub-dictionary from JSON
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

instance FromJSON Participant where
    parseJSON = withObject "participant" $ \o -> do
        p_participantId <- o .: "participantId"
        p_teamId <- o .: "teamId"
        p_championId <- o .: "championId"
        stats <- o .: "stats"
        -- Now parse the stats from the stats subdictionary
        p_win <- stats .: "win"
        p_kills <- stats .: "kills"
        p_deaths <- stats .: "deaths"
        p_assists <- stats .: "assists"
        p_largestKillingSpree <- stats .: "largestKillingSpree"
        p_largestMultiKill <- stats .: "largestMultiKill"
        p_totalDamageDealt <- stats .: "totalDamageDealt"
        p_totalDamageDealtToChampions <- stats .: "totalDamageDealtToChampions"
        p_totalDamageTaken <- stats .: "totalDamageTaken"
        p_goldEarned <- stats .: "goldEarned"
        p_goldSpent <- stats .: "goldSpent"
        p_totalMinionsKilled <- stats .: "totalMinionsKilled"
        return Participant{..}

instance ToJSON Participant where
    toJSON p = object [
        "participantId" .= p_participantId p,
        "teamId" .= p_teamId p,
        "championId" .= p_championId p,
        "stats" .= object [
            "win" .= p_win p,
            "kills" .= p_kills p,
            "deaths" .= p_deaths p,
            "assists" .= p_assists p,
            "largestKillingSpree" .= p_largestKillingSpree p,
            "largestMultiKill" .= p_largestMultiKill p,
            "totalDamageDealt" .= p_totalDamageDealt p,
            "totalDamageDealtToChampions" .= p_totalDamageDealtToChampions p,
            "totalDamageTaken" .= p_totalDamageTaken p,
            "goldEarned" .= p_goldEarned p,
            "goldSpent" .= p_goldSpent p,
            "totalMinionsKilled" .= p_totalMinionsKilled p
            ]
        ]


-- Automatically derive To/From JSON for Team and Match Types - stripping the leading identifiers.
$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Team)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Match)

{- |
    'parseMatch' takes a 'ByteString' type, parses it to a 'Match' type - including all of its sub-components.
    
    By definition of a 'Match' in League of Legends, a single 'Match' will contain 10 'Participant' objects, 
        10 'ParticipantIdentity' objects and 2 'Team' objects.
-}
parseMatch :: L8.ByteString -> Either String Match
parseMatch json = eitherDecode json :: Either String Match