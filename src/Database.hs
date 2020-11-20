{-|
Module      : Database
Stability   : experimental
Portability : POSIX

This module is responsible for creating and maintaining the connection to the database.
    This modules exports methods which create the database, 'initialiseDB', as well as 
    different methods with which to write the Haskell Datatypes defined in the 'Parse'
    module to the database.
-}

module Database
    (
      Connection,
      initialiseDB,
      -- * Functions to save to the database
      saveSummoner,
      saveMatch,
      -- * Functions to query the database
      queryAccountIdByName
    ) where

-- Import required modules
import Database.HDBC
import Database.HDBC.Sqlite3
import Parse

{- |
    'initialiseDB' creates a new 'Connection' to the SQL database at "./league.sqlite", and
        ensures that all of the required tables are created (in the case the database has 
        been wiped or deleted)

    This takes no arguments, and returns an 'IO' 'Connection' type, which can be used to 
        interface with the Database.
-}
initialiseDB :: IO Connection
initialiseDB = 
    do
        conn <- connectSqlite3 "league.sqlite"
        -- Each table, and commit them to the database
        run conn "CREATE TABLE IF NOT EXISTS summoners (\
            \id VARCHAR(80) NOT NULL, \
            \accountId VARCHAR(80) NOT NULL, \
            \puuid VARCHAR(80) NOT NULL, \
            \name VARCHAR(50) NOT NULL, \
            \profileIconId INT DEFAULT NULL, \
            \revisionDate INT DEFAULT NULL, \
            \summonerLevel INT DEFAULT NULL, \
            \PRIMARY KEY (id) \
            \)" []
        commit conn
        run conn "CREATE TABLE IF NOT EXISTS match (\
            \gameId INT DEFAULT NULL, \
            \gameCreation INT DEFAULT NULL, \
            \gameDuration INT DEFAULT NULL, \
            \gameMode VARCHAR(40) NOT NULL, \
            \gameType VARCHAR(40) NOT NULL, \
            \PRIMARY KEY (gameId) \
            \)" []
        commit conn
        run conn "CREATE TABLE IF NOT EXISTS team (\
            \gameId INT DEFAULT NULL, \
            \teamId INT DEFAULT NULL, \
            \win VARCHAR(10) NOT NULL, \
            \firstBlood INT DEFAULT NULL, \
            \firstTower INT DEFAULT NULL, \
            \towerKills INT DEFAULT NULL, \
            \inhibitorKills INT DEFAULT NULL, \
            \baronKills INT DEFAULT NULL, \
            \dragonKills INT DEFAULT NULL, \
            \riftHeraldKills INT DEFAULT NULL, \
            \PRIMARY KEY (gameId, teamId) \
            \)" []
        commit conn
        run conn "CREATE TABLE IF NOT EXISTS participant (\
            \gameId INT DEFAULT NULL, \
            \participantId INT DEFAULT NULL, \
            \teamId INT DEFAULT NULL, \
            \championId INT DEFAULT NULL, \
            \win INT DEFAULT NULL, \
            \kills INT DEFAULT NULL, \
            \deaths INT DEFAULT NULL, \
            \assists INT DEFAULT NULL, \
            \largestKillingSpree INT DEFAULT NULL, \
            \largestMultiKill INT DEFAULT NULL, \
            \totalDamageDealt INT DEFAULT NULL, \
            \totalDamageDealtToChampions INT DEFAULT NULL, \
            \totalDamageTaken INT DEFAULT NULL, \
            \goldEarned INT DEFAULT NULL, \
            \goldSpent INT DEFAULT NULL, \
            \totalMinionsKilled INT DEFAULT NULL, \
            \PRIMARY KEY (gameId, participantId) \
            \)" []
        commit conn
        run conn "CREATE TABLE IF NOT EXISTS participantIdentity (\
            \gameId INT DEFAULT NULL, \
            \participantId INT DEFAULT NULL, \
            \accountId VARCHAR(80) NOT NULL, \
            \summonerName VARCHAR(50) NOT NULL, \
            \summonerId VARCHAR(80) NOT NULL, \
            \currentAccountId VARCHAR(80) NOT NULL, \
            \PRIMARY KEY (gameId, participantId) \
            \)" []
        commit conn
        return conn

-- Functions to convert our Haskell records to SQL types from a given record:
{- |
    'summonerToSQL' converts the Haskell Datatype 'Summoner', to the relevent 'SqlValue' types which 
        we can use to insert to the database. 
-}
summonerToSQL :: Summoner -> [SqlValue]
summonerToSQL summoner = [
        toSql $ s_id summoner,
        toSql $ s_accountId summoner,
        toSql $ s_puuid summoner,
        toSql $ s_name summoner,
        toSql $ s_profileIconId summoner,
        toSql $ s_revisionDate summoner,
        toSql $ s_summonerLevel summoner
    ]

{- |
    'prepareSummonerInsert' prepares the SQL insert statement, without the explicit values.
        This function takes the database 'Connection' and returns an 'IO' 'Statement' which is 
        ready to have values inserted.
-}
prepareSummonerInsert :: Connection -> IO Statement
prepareSummonerInsert conn = prepare conn "INSERT INTO summoners VALUES (?,?,?,?,?,?,?)"

{- |
    'saveSummoner' takes a 'Summoner' type and 'Connection' as arguments. This function prepares
        the insert statement, and then executes it with the values from the given summoner which
        has been converted to 'SqlValue' types with 'summonerToSQL'.
    
    This function also performs a check to ensure it will not attempt to write to a primary key
        (summoner_id) that is already occupied.
-}
saveSummoner :: Summoner -> Connection -> IO ()
saveSummoner summoner conn = do
    let summoner_id = s_id summoner
    let query = "SELECT * FROM summoners WHERE id='" ++ summoner_id ++ "'"
    check_exists <- quickQuery' conn query []
    if check_exists == [] then do
        statement <- prepareSummonerInsert conn
        execute statement $ summonerToSQL summoner
        commit conn
        print "Summoner saved to database successfully."
    else 
        print "Summoner already exists in database!"

-- Functions relating to Match Types:
{- |
    'participantIdentityToSQL' takes a 'ParticipantIdentity' type and the given game's id, and
        returns a list of 'SqlValue' objects for the respective table
-}
participantIdentityToSQL :: Int -> ParticipantIdentity -> [SqlValue]
participantIdentityToSQL game_id participant_identity = [
        toSql game_id,
        toSql $ pi_participantId participant_identity,
        toSql $ pi_accountId participant_identity,
        toSql $ pi_summonerName participant_identity,
        toSql $ pi_summonerId participant_identity,
        toSql $ pi_currentAccountId participant_identity
    ]

{- |
    'participantToSQL' takes the respective game_id and a 'Participant' type, and returns the list
        of respective 'SqlValue' objects, needed for the SQLite table.
-}
participantToSQL :: Int -> Participant -> [SqlValue]
participantToSQL game_id participant = [
        toSql game_id,
        toSql $ p_participantId participant,
        toSql $ p_teamId participant,
        toSql $ p_championId participant,
        toSql $ p_win participant,
        toSql $ p_kills participant,
        toSql $ p_deaths participant,
        toSql $ p_assists participant,
        toSql $ p_largestKillingSpree participant,
        toSql $ p_largestMultiKill participant,
        toSql $ p_totalDamageDealt participant,
        toSql $ p_totalDamageDealtToChampions participant,
        toSql $ p_totalDamageTaken participant,
        toSql $ p_goldEarned participant,
        toSql $ p_goldSpent participant,
        toSql $ p_totalMinionsKilled participant
    ]

{- |
    'teamToSQL' takes the given game Id, and a 'Team' type, and returns the respective
        list of 'SqlValue' objects for the 'Team' table in the SQLite database.
-}
teamToSQL :: Int -> Team -> [SqlValue]
teamToSQL game_id team = [
        toSql game_id,
        toSql $ t_teamId team,
        toSql $ t_win team,
        toSql $ t_firstBlood team,
        toSql $ t_firstTower team, 
        toSql $ t_towerKills team,
        toSql $ t_inhibitorKills team,
        toSql $ t_baronKills team,
        toSql $ t_dragonKills team,
        toSql $ t_riftHeraldKills team
    ]

{- |
    'matchToSQL' takes a match object, and parses the required values for the match table
    in the match database table.
-}
matchToSQL :: Match -> [SqlValue]
matchToSQL match = [
        toSql $ m_gameId match,
        toSql $ m_gameCreation match,
        toSql $ m_gameDuration match,
        toSql $ m_gameMode match,
        toSql $ m_gameType match
    ]

{- |
    'saveMatch' takes a 'Match' object, and prepares each of the required statements for its
    components for saving to the database. It will save 10 'Participant' and 'ParticipantIdentity'
    records, 2 'Team' records and one overall 'Match' to the database. 

    This method will not perform any meaningful action if the match that is being written to the
    database already exists, by performing a quick query on the primary key - the game_id.
-}
saveMatch :: Match -> Connection -> IO ()
saveMatch match conn = do
    let game_id = m_gameId match
    check_exists <- quickQuery' conn ("SELECT * FROM match WHERE gameId=" ++ show(game_id)) []
    if check_exists == [] then do
        match_statement <- prepare conn "INSERT INTO match VALUES (?,?,?,?,?)"
        execute match_statement $ matchToSQL match
        team_statement <- prepare conn "INSERT INTO team VALUES (?,?,?,?,?,?,?,?,?,?)"
        executeMany team_statement $ map (teamToSQL game_id) (m_teams match)
        p_statement <- prepare conn "INSERT INTO participant VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        executeMany p_statement $ map (participantToSQL game_id) (m_participants match)
        pi_statement <- prepare conn "INSERT INTO participantIdentity VALUES (?,?,?,?,?,?)"
        executeMany pi_statement $ map (participantIdentityToSQL game_id) (m_participantIdentities match)
        commit conn
        print "Match saved to database successfully."
    else
        print "Match already exists!"

{- |
    'queryAccountIdByName' is a function which takes a 'Connection' and a 'String' - denoting the name
        Account/Summoner being queried.

    This function returns the 'String' giving the queried accounts accountId given a successful query on
        the database. In the case that no results, or more than a single result is returned - meaning our
        query was ambiguous - we return a Left 'Either' denoting the error.
-}
queryAccountIdByName :: Connection -> String -> IO (Either String String)
queryAccountIdByName conn account_name = do
    values <- quickQuery' conn ("SELECT accountId FROM summoners WHERE name='" ++ account_name ++ "'") []
    case (length values) of
        1 -> do
            let id = fromSql (head (head values)) :: String
            return (Right id)
        _ -> do
            return (Left "Query returned unexpected number of results (0 or, 2 or more)")
