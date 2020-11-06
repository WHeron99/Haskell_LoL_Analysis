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
        -- Create the Summoners table (for accounts)
        run conn "CREATE TABLE IF NOT EXISTS summoners (\
            \id VARCHAR(60) NOT NULL, \
            \accountId VARCHAR(60) NOT NULL, \
            \puuid VARCHAR(60) NOT NULL, \
            \name VARCHAR(40) NOT NULL, \
            \profileIconId INT DEFAULT NULL, \
            \revisionDate INT DEFAULT NULL, \
            \summonerLevel INT DEFAULT NULL \
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