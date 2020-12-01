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
        \3. Print all Summoners/Accounts in the database \n\
        \4. Query the most popular game mode among stored matches \n\
        \5. Get list of all Summoners, sorted by kill count \n\
        \6. Display participants in order of damage dealt and taken \n\
        \9. Dump Database to JSON files \n\
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
        "3" -> dispatchGetAllSummoners conn
        "4" -> dispatchQueryMostPlayedGameMode conn
        "5" -> dispatchSummonerWithMostKills conn
        "6" -> dispatchSummonerByDamage conn
        "9" -> dumpDatabaseToJSON conn
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
        their 10 latest matches from the API, to add to the database.

    If the Summoner already exists in the database, their account id will be retrieved from the database using
        a simple query, and then passed to 'fetchAndSaveMatches'.

    In the case the Summoner is not in the database, an attempt will be made to query the API for the given
        Summoner given the name, and if successful, the function will continue to execute by taking the returned
        Summoner's account ID for use with 'fetchAndSaveMatches' function
-}
dispatchGetPlayerRecentMatches :: Connection -> IO ()
dispatchGetPlayerRecentMatches conn = do
    putStrLn "Please enter the name of the Summoner/Account you would like to get the matches for: "
    account_name <- getLine

    result <- queryAccountIdByName conn account_name
    -- putStrLn $ show result -- ? Test Line - DEBUG: Shows the result of the query

    case result of
        Left str -> do
            -- Queried summoner does not currently exist in database - get them and add to database before resuming.
            putStrLn "Given summoner not current in database... Fetching..."
            summoner_json <- requestSummonerByName account_name
            case (parseSummoner summoner_json) of 
                Left err -> do
                    putStrLn "!! Summoner does not exist, or API endpoint is down. Please try again, and if the error persists try another Summoner. !!"
                Right summoner -> do
                    -- We have the summoner object, save it to database, and use their account id to save their match history
                    saveSummoner conn summoner
                    fetchAndSavePlayersMatches conn $ s_accountId summoner
        Right account_id -> do
            -- Account ID Successfully retrieved, call for their match list
            fetchAndSavePlayersMatches conn account_id


{- |
    'fetchAndSavePlayersMatches' acts as a helper method to 'dispatchGetPlayerRecentMatches', by taking the active
        'Connection' and an account ID, given by a 'String' in order to request their 10 most recent matches, parse
        them in to the 'Match' type, and then save to the database.
-}
fetchAndSavePlayersMatches :: Connection -> String -> IO ()
fetchAndSavePlayersMatches conn account_id = do
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


{- |
    'dispatchGetAllSummoners' is called when the user wishes to display a list of each of the Summoners they have
        entered in to the database. A call is made to the respective database function, which passes the results
        back to the function, such that we can print each to standard input - utilising mapM_ as we are only
        interested in the side-effect of printing.
-}
dispatchGetAllSummoners :: Connection -> IO ()
dispatchGetAllSummoners conn = do
    res <- queryAllSummoners conn
    putStrLn "Summoners currently stored in the database: "
    mapM_ (\(x,y) -> putStrLn $ x ++ ", Summoner Level: " ++ show y) res


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

{- |
    'dispatchSummonerWithMostKills' is called when the user wishes to display a list of participants from all matches, 
        ordered by the number of kills they got in all of their games. This function simply takes the database
        'Connection' with which to query on, and makes a call to the Database module to get the results in a given
        format, which are then read back via standard output for each tuple.
-}
dispatchSummonerWithMostKills :: Connection -> IO ()
dispatchSummonerWithMostKills conn = do
    res <- querySummonerWithMostKills conn
    putStrLn "Summoners with most kills in stored games: "
    mapM_ (\(x, y) -> putStrLn $ x ++ ": " ++ show y) res 


{- |
    'dispatchSummonerByDamage' is called when the user wishes to display each participant, ordered by the average damage
        they have dealt and taken. This function simply passes the 'Connection' to the respective Database function, 
        and then iterates through the returned tuples to display the results of the query.
-}
dispatchSummonerByDamage :: Connection -> IO ()
dispatchSummonerByDamage conn = do
    res <- querySummonerByDamage conn
    putStrLn "Summoner Name | Average Damage Dealt | Average Damage Taken"
    mapM_ (\(x, y, z) -> putStrLn $ x ++ ": " ++ show y ++ " | " ++ show z) res