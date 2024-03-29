{-|
Module      : HTTPS
Stability   : experimental
Portability : POSIX

This module is responsible for attempting to make API calls to a given API endpoint, using
a set of predefined functions for each particular interaction (i.e. retrieving a Match, or Summoner)
-}

module HTTPS
    (
      makeAPIRequest,
      requestSummonerByName,
      requestMatchList,
      requestMatchData
    ) where

-- Import needed packages
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

-- Alias for our URL
type URL = String

-- The API key we need to make requests to the Riot Games API, add it to a query parameter
api_key = "xxx"
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

{- |
  'requestMatchList' takes a players account id, and attempts to query the match endpoint on the Riot
    Games API, to get their recent match list - with a limit of 10 matches.

  This function takes a 'String' parameter, containing the accounts accountId.

  This function returns an 'IO' 'L8.ByteString', which contains the JSON from the HTTP response body.
-}
requestMatchList :: String -> IO L8.ByteString
requestMatchList accountId = do
  req <- parseRequest $ "https://euw1.api.riotgames.com/lol/match/v4/matchlists/by-account/" ++ accountId ++ "?endIndex=10&api_key=" ++ api_key
  res <- httpLBS req
  return $ getResponseBody res


{- |
  'requestMatchData' takes a match id, and attempts to query the match endpoint for detailed match
    information. 

  This function takes an 'Int' parameter - which is the match ID to request.

  This function returns an 'IO' 'L8.ByteString', containing all of the match details.
-}
requestMatchData :: Int -> IO L8.ByteString
requestMatchData game_id = do
  req <- parseRequest $ "https://euw1.api.riotgames.com/lol/match/v4/matches/" ++ show(game_id) ++ "?api_key=" ++ api_key
  res <- httpLBS req
  return $ getResponseBody res
