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
      queryAccountIdByName,
      queryMostPlayedGameMode,
      querySummonerWithMostKills,
      -- * Dump functionality
      dumpDatabaseToJSON
    ) where

-- Import required modules
import Database.HDBC
import Database.HDBC.Sqlite3
import Parse

import qualified Data.ByteString.Lazy.Char8 as L8

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
saveSummoner :: Connection -> Summoner -> IO ()
saveSummoner conn summoner = do
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
saveMatch :: Connection -> Match -> IO ()
saveMatch conn match = do
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
    'queryAccountIdByName' is a function which takes a 'Connection' to the database and a 'String' - denoting the name
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
            return (Left "Query returned an unexpected number of results")


{- |
    'queryMostPlayedGameMode' is a function that executes a query on the database linked via the given 'Connection' in order
        to return each game mode present in the collection of matches in the database, ordered by their popularity in the
        collection.

    The return format of [('String', 'Int')] denotes a list of tuples, where each tuple represents a game mode, given by
        the 'String', and its popularity, in terms of the number of matches played of that mode in the database, given by
        the 'Int'.
-}
queryMostPlayedGameMode :: Connection -> IO ([(String, Int)])
queryMostPlayedGameMode conn = 
    do
        res <- quickQuery' conn "SELECT m.gameMode, COUNT(*) FROM match m GROUP BY m.gameMode ORDER BY COUNT(*) DESC" []
        let matchTypes = map convertFromSql res
        return matchTypes
    where
        convertFromSql :: [SqlValue] -> (String, Int)
        convertFromSql [sql_match_type, sql_count] = (match_type, count)
            where
                match_type = case fromSql sql_match_type of
                    Just x -> x
                    Nothing -> "NULL"
                count = (fromSql sql_count) :: Int

{- |
    'querySummonerWithMostKills' is a function which will execute a query on the database at the given 'Connection', and
        return a list of tuples for the top 5 players, based on the number of kills they have in all of the stored matches.
        The SQL query is read in from a local file, and executed, and the returned SqlValues parsed.

    The returned tuple has the form of ('String', 'Int'), which gives the name of Summoner, and the number of kills they scored.
-}
querySummonerWithMostKills :: Connection -> IO ([(String, Int)])
querySummonerWithMostKills conn =
    do
        sql_query <- readFile "sql/summoner_with_most_kills.sql"
        res <- quickQuery' conn sql_query []
        let pairs = map convertFromSql res
        return pairs
    where
        convertFromSql :: [SqlValue] -> (String, Int)
        convertFromSql [sql_summoner_name, sql_kill_count] = (summoner_name, kill_count)
            where  
                summoner_name = parseSqlString sql_summoner_name
                kill_count = (fromSql sql_kill_count) :: Int


{- |
    'dumpDatabaseToJSON' dumps the entire contents of the Database in to two JSON files (for both Matches and Summoners).
        The required queries are read from a local file, which utilises the JSON1 extension for SQLite to read the results
        of some general queries to a JSON string, which is then returned as an SqlValue, and parsed in to a standard 'L8.ByteString'
        which can be written to a file in the ./OUT/ directory.
-}
dumpDatabaseToJSON :: Connection -> IO ()
dumpDatabaseToJSON conn = 
    do
        -- Dump the matches to a JSON file
        dump_matches <- readFile "sql/dump_matches.sql"
        res <- quickQuery' conn dump_matches []
        let json = fromSql (head (head res)) :: L8.ByteString
        L8.writeFile "OUT/matches_dump.json" json

        -- Dump the summoners to their own JSON file
        dump_summoners <- readFile "sql/dump_summoners.sql"
        res' <- quickQuery' conn dump_summoners []
        let json' = fromSql (head (head res')) :: L8.ByteString
        L8.writeFile "OUT/summoners_dump.json" json'


{- |
    'parseSqlString' is a helper function, which takes an 'SqlValue' type, and attempts to
        parse it in to a String. Should the parse fail, it returns the 'String' "NULL", else
        it simply returns the value of the 'SqlValue' String itself.
-}
parseSqlString :: SqlValue -> String
parseSqlString sql_str = 
    case fromSql sql_str of
        Just x -> x
        Nothing -> "NULL"