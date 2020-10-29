module HTTPS
    ( someFunc,
      makeAPIRequest
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type URL = String

-- Attempt to make an API request on the given URL, and return the bytestring from the response body.
makeAPIRequest :: URL -> IO L8.ByteString
makeAPIRequest url = do
    req <- parseRequest url
    response <- httpLBS req
    return $ getResponseBody response

