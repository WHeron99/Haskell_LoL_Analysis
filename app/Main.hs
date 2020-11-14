module Main where

import HTTPS
import Parse
import Database

main :: IO ()
main = do
    conn <- initialiseDB
    print "Database connection established..."
    print "Please enter a Summoner name to retrieve..."
    name <- getLine
    print "Downloading..."
    json <- requestSummonerByName name
    print "Parsing..."
    case (parseSummoner json) of 
        Left err -> print err
        Right summoner -> do
            print "Saving to DB..."
            saveSummoner summoner conn
    -- ! TEST FOR MATCHLIST PARSE
    let url' = "https://euw1.api.riotgames.com/lol/match/v4/matches/4882690236"
    json' <- makeAPIRequest url'
    case (parseMatch json') of
        Left err -> print err
        Right match -> do
            saveMatch match conn
