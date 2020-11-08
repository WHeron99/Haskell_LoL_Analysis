module Database
    (
      initialiseDB,
      saveSummoner
    ) where

-- Import required modules
import Database.HDBC
import Database.HDBC.Sqlite3
import Parse

initialiseDB :: IO Connection
initialiseDB = 
    do
        conn <- connectSqlite3 "league.sqlite"
        -- Each table, and commit them to the database
        run conn "CREATE TABLE IF NOT EXISTS summoners (\
            \id VARCHAR(60) NOT NULL, \
            \accountId VARCHAR(60) NOT NULL, \
            \puuid VARCHAR(60) NOT NULL, \
            \name VARCHAR(40) NOT NULL, \
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
-- /summonerToSQL converts the Haskell Datatype Summoner, to the relevent SQL values which
-- we can use to insert to the database.
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

-- /prepareSummonerInsert prepares the SQL insert statement, without the explicit values.
prepareSummonerInsert :: Connection -> IO Statement
prepareSummonerInsert conn = prepare conn "INSERT INTO summoners VALUES (?,?,?,?,?,?,?)"

-- /saveSummoner takes a Summoner type and connection as arguments. This function prepares
-- the insert statement, and then executes it with the values from the given summoner which
-- has been converted to SqlValues with summonerToSQL.
saveSummoner :: Summoner -> Connection -> IO ()
saveSummoner summoner conn = do
    statement <- prepareSummonerInsert conn
    execute statement $ summonerToSQL summoner
    commit conn

-- Functions relating to Match Type:
-- TODO matchToSQL :: Match -> [SqlValue]