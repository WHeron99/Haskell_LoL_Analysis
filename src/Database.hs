module Database
    (
      initialiseDB
    ) where

-- Import required modules
import Database.HDBC
import Database.HDBC.Sqlite3
import Parse

initialiseDB :: IO Connection
initialiseDB = 
    do
        conn <- connectSqlite3 "league.sqlite"
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
-- TODO leaderboardToSQL :: Leaderboard -> [SqlValue]

-- TODO leagueEntryToSQL :: LeagueEntry -> [SqlValue]

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