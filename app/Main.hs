module Main where

import HTTPS
import Parse
import Database

main :: IO ()
main = do
    let url = "https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/Firesoulpwn"
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
