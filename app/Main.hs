module Main where

import HTTPS
import Parse
import Database

import Control.Monad

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
        "3" -> dispatchQueryMostPlayedGameMode conn
        _ -> putStrLn "Sorry, I do not recognise that command, please try again."
    
    -- Check exit condition - Repeat if they didn't choose to exit the program
    case userChoice of
        "0" -> return ()
        _ -> displayUserChoices conn


{- |
    'dispatchGetNewSummoner' is a function which requests the user to input the name of a Summoner/Account
    to add to the database. The user is presented a prompt to enter the name of the summoner, and on providing
    a name, the program will attempt to query the API and write a successful query result to the database.
-}
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
            saveSummoner conn summoner


{-
    'dispatchGetPlayerRecentMatches' prompts the user to input the name of a Summoner with which to query
    their 10 latest matches from the API, to add to the database. In this case, the Summoner must already
    exist in the local database, else the user will get a warning that the query to find the summoner returned
    no or too many results. 
-}
dispatchGetPlayerRecentMatches :: Connection -> IO ()
dispatchGetPlayerRecentMatches conn = do
    putStrLn "Please enter the name of the Summoner/Account you would like to get the matches for: "
    account_name <- getLine

    result <- queryAccountIdByName conn account_name
    -- putStrLn $ show result -- ? Test Line - DEBUG: Shows the result of the query

    case result of
        Left str -> putStrLn str
        Right account_id -> do
            -- Account ID Successfully retrieved, call for their match list
            json <- requestMatchList account_id
            case (parseMatchList json) of
                Left err -> do
                    putStrLn err
                Right match_list -> do
                    let match_ids = map (mi_gameId) (ml_matches match_list)
                    matches <- mapM (requestMatchData) (match_ids)
                    -- Following line determines if all matches were retrieved succesfully.
                    let parsed_matches = mapM parseMatch matches
                    case parsed_matches of 
                        Left err -> print err
                        Right matches' -> do
                            -- Matches successfully parsed, write them all to DB (Use mapM_ to discard the results of the function)
                            mapM_ (saveMatch conn) matches'


{-
    'dispatchQueryMostPlayedGameMode' is called when the user wishes to determine which game mode is most popular
        in the database when presented the list of potential options. This function simply takes the database 
        'Connection', and utilises the function from the Database module to get the results based on a predefined
        query.

    The results are returned as a tuple, and mapped over in order to print them in the order as given by the query,
        which is in descending order based on the number of matches played of that game mode in the database.
-}
dispatchQueryMostPlayedGameMode :: Connection -> IO ()
dispatchQueryMostPlayedGameMode conn = do
    res <- queryMostPlayedGameMode conn
    putStrLn "Most popular game modes: "
    -- Map over the tuples to print each - mapM_ to discard the implied result of the operation.
    mapM_ (\(x,y) -> putStrLn $ x ++ ": " ++ show y) res
