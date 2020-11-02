module Main where

import HTTPS
import Parse

main :: IO ()
main = do
    let url = "https://euw1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5"
    json <- makeAPIRequest url
    print json
