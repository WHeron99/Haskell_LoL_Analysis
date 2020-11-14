{-|
Module      : HTTPS
Stability   : experimental
Portability : POSIX

This module is responsible for attempting to make API calls to a given API endpoint, using
a set of predefined functions.
-}

module HTTPS
    (
      makeAPIRequest,
      requestSummonerByName
    ) where

-- Import needed packages
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

-- Alias for our URL
type URL = String

-- The API key we need to make requests to the Riot Games API, add it to a query parameter
api_key = "RGAPI-b203c052-065b-4e17-88a7-cce9cc8c551d"
api_key_query_param = "?api_key=" ++ api_key
 
{- |
  'makeAPIRequest' takes a 'URL' - which is an alias for 'String', and attempts to query the given API URL.

  This function should return an 'IO' 'L8.ByteString' denoting the returned JSON data from the HTTP response body.
-}
makeAPIRequest :: URL -> IO L8.ByteString
makeAPIRequest url = do
    req <- parseRequest $ url ++ api_key_query_param                  -- Create the request to the given URL + the API key as a parameter
    response <- httpLBS req                                           -- Make the request with the Bytestring return type
    return $ getResponseBody response                                 -- Return the request body

{- |
  'requestSummonerByName' takes a players/accounts name, and attempts to query the summoners end point
  of the Riot Games API to get their Summoner (Account) information.

  This function takes a single 'String' parameter, which denotes the name of the account we are looking
  for.

  This function returns an 'IO' 'L8.ByteString', which contains the returned JSON data from the HTTP response 
  body.
-}
requestSummonerByName :: String -> IO L8.ByteString
requestSummonerByName name = do
    req <- parseRequest $ "https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/" ++ name ++ "?api_key=" ++ api_key
    res <- httpLBS req
    return $ getResponseBody res
