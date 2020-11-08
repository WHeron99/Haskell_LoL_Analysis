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
api_key = "RGAPI-b203c052-065b-4e17-88a7-cce9cc8c551d"
api_key_query_param = "?api_key=" ++ api_key
 
-- /The makeAPIRequest function attempts to make an API request on the given URL,
-- and returns the bytestring from the response body, if the request is succesful.
makeAPIRequest :: URL -> IO L8.ByteString
makeAPIRequest url = do
    req <- parseRequest $ url ++ api_key_query_param                  -- Create the request to the given URL + the API key as a parameter
    response <- httpLBS req                                           -- Make the request with the Bytestring return type
    return $ getResponseBody response                                 -- Return the request body
