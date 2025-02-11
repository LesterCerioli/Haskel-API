module Infrastructure.Blockchains.Algorand where

import Network.HTTP.Simple
import Data.Aeson (decode, encode, object, (.=))
import Control.Monad (void)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

-- Function to load environment-specific variables
getEnvVariable :: String -> IO String
getEnvVariable var = do
    value <- lookupEnv var
    return $ fromMaybe "" value

-- Function to get Algorand API URL based on selected environment
getAlgorandApiUrl :: IO String
getAlgorandApiUrl = do
    env <- getEnvVariable "APP_ENV" -- "development" or "production"
    if env == "production"
        then getEnvVariable "ALGORAND_API_URL_PROD"
        else getEnvVariable "ALGORAND_API_URL_DEV"

-- Function to get Algorand API Key based on selected environment
getAlgorandApiKey :: IO String
getAlgorandApiKey = do
    env <- getEnvVariable "APP_ENV"
    if env == "production"
        then getEnvVariable "ALGORAND_API_KEY_PROD"
        else getEnvVariable "ALGORAND_API_KEY_DEV"

-- Function to get ALGO balance from an Algorand address
getBalance :: String -> IO (Maybe Double)
getBalance address = do
    apiUrl <- getAlgorandApiUrl
    apiKey <- getAlgorandApiKey
    let request = setRequestHeader "X-API-Key" [apiKey]
                  $ parseRequest_ (apiUrl ++ "/v2/accounts/" ++ address)
    response <- httpJSON request
    return $ decode (getResponseBody response)

-- Function to send ALGO between accounts (internal or external)
sendTransaction :: String -> String -> Double -> Bool -> IO ()
sendTransaction from to amount sameOwner = do
    apiUrl <- getAlgorandApiUrl
    apiKey <- getAlgorandApiKey
    if sameOwner
        then putStrLn $ "Internal Algorand Transfer: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
        else do
            putStrLn $ "Sending Algorand Transaction: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
            let payload = encode $ object ["from" .= from, "to" .= to, "amount" .= amount]
            let request = setRequestHeader "X-API-Key" [apiKey]
                          $ setRequestBodyJSON payload
                          $ parseRequest_ (apiUrl ++ "/v2/transactions")
            void $ httpJSON request
