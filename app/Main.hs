module Main where

import HTTPS

main :: IO ()
main = do
    let url = "https://opendata.ecdc.europa.eu/covid19/casedistribution/json/"
    json <- makeAPIRequest url
    print json
