module Main where

import HTTPS
import Parse
import Database

main :: IO ()
main = do
    print "Please enter a Summoner name to retrieve..."
    name <- getLine
    let url = "https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/" ++ name
    print "Downloading..."
    json <- makeAPIRequest url
    print "Parsing..."
    case (parseSummoner json) of 
        Left err -> print err
        Right summoner -> do
            print "Saving to DB..."
            conn <- initialiseDB
            saveSummoner summoner conn
    print "Fin."
    -- ! TEST FOR MATCHLIST PARSE
    let url' = "https://euw1.api.riotgames.com/lol/match/v4/matches/4882690236"
    json' <- makeAPIRequest url'
    case (parseMatch json') of
        Left err -> print err
        Right match -> do
            print match
