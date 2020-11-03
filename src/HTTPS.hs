module HTTPS
    (
      makeAPIRequest
    ) where

-- Import needed packages
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

-- Alias for our URL
type URL = String

-- The API key we need to make requests to the Riot Games API, add it to a query parameter
api_key = "RGAPI-8f87f623-91c0-4e11-90d7-40ebcd231cea"
api_key_query_param = "?api_key=" ++ api_key
 
-- Attempt to make an API request on the given URL, and return the bytestring from the response body.
makeAPIRequest :: URL -> IO L8.ByteString
makeAPIRequest url = do
    req <- parseRequest $ url ++ api_key_query_param                  -- Create the request to the given URL + the API key as a parameter
    response <- httpLBS req                                           -- Make the request with the Bytestring return type
    return $ getResponseBody response                                 -- Return the request body