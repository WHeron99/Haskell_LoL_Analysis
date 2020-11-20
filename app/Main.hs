module Main where

import HTTPS
import Parse
import Database

main :: IO ()
main = do
    putStrLn "--- Welcome to Will and Brandon's Haskell App! ---"
    conn <- initialiseDB
    putStrLn "Database connection established..."
    displayUserChoices conn


{- |
'displayerUserChoices' displays a list of actions that the user may take with the application, and then
    provides them the option to select one, and based on their selection, dispatch the required function
    to perform the action. This function will be called recursively until the user decides to quit, in
    which case this function simply does not call itself any further.
-}
displayUserChoices :: Connection -> IO ()
displayUserChoices conn = do
    let menu_text = "\nWhat would you like to do? \n\
        \1. Add a new account to the database \n\
        \2. Add a players 10 most recent matches to the database \n\
        \3. Query the most popular game mode among stored matches \n\
        \4. (Etc. I'll think of more later...) \n\
        \0. --- QUIT --- \n\n\
        \Please input your choice (as a number):\n\
        \"
    putStrLn menu_text

    -- Dispatch relevant function -- TODO - Improve to use a dispatch type or something more expandable
    userChoice <- getLine
    case userChoice of
        "0" -> putStrLn "Thanks for using our program. Goodbye!"
        "1" -> dispatchGetNewSummoner conn 
        "2" -> dispatchGetPlayerRecentMatches conn
        _ -> putStrLn "Sorry, I do not recognise that command, please try again."
    
    -- Check exit condition
    case userChoice of
        "0" -> return ()
        _ -> displayUserChoices conn


dispatchGetNewSummoner :: Connection -> IO ()
dispatchGetNewSummoner conn = do
    putStrLn "Please enter a Summoner name to retrieve..."
    name <- getLine
    putStrLn "Downloading..."
    json <- requestSummonerByName name
    putStrLn "Parsing..."
    case (parseSummoner json) of 
        Left err -> do
            putStrLn "!! Summoner does not exist, or API endpoint is down. Please try again, and if the error persists try another Summoner. !!"
        Right summoner -> do
            putStrLn "Saving to DB..."
            saveSummoner summoner conn

dispatchGetPlayerRecentMatches :: Connection -> IO ()
dispatchGetPlayerRecentMatches conn = do
    -- ! EXPERIMENTAL ! -- 
    {- TODO : Allow user to select a Player with which to get their 10 most recent matches,
                first check if their details are in the database, if they are, use their stored
                credentials to query their match list - then iterate over them to retrieve and store
                full match details.
    -}
    let url' = "https://euw1.api.riotgames.com/lol/match/v4/matches/4882690236"
    json' <- makeAPIRequest url'
    case (parseMatch json') of
        Left err -> putStrLn err
        Right match -> do
            saveMatch match conn