module Infrastructure.Blockchains.Cardano where

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

-- Function to get Cardano API URL based on selected environment
getCardanoApiUrl :: IO String
getCardanoApiUrl = do
    env <- getEnvVariable "APP_ENV" -- "development" or "production"
    if env == "production"
        then getEnvVariable "CARDANO_API_URL_PROD"
        else getEnvVariable "CARDANO_API_URL_DEV"

-- Function to get Cardano API Key based on selected environment
getCardanoApiKey :: IO String
getCardanoApiKey = do
    env <- getEnvVariable "APP_ENV"
    if env == "production"
        then getEnvVariable "CARDANO_API_KEY_PROD"
        else getEnvVariable "CARDANO_API_KEY_DEV"

-- Function to get ADA balance from a Cardano address
getBalance :: String -> IO (Maybe Double)
getBalance address = do
    apiUrl <- getCardanoApiUrl
    apiKey <- getCardanoApiKey
    let request = setRequestHeader "project_id" [apiKey]
                  $ parseRequest_ (apiUrl ++ "/addresses/" ++ address)
    response <- httpJSON request
    return $ decode (getResponseBody response)

-- Function to send ADA between accounts (internal or external)
sendTransaction :: String -> String -> Double -> Bool -> IO ()
sendTransaction from to amount sameOwner = do
    apiUrl <- getCardanoApiUrl
    apiKey <- getCardanoApiKey
    if sameOwner
        then putStrLn $ "Internal Cardano Transfer: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
        else do
            putStrLn $ "Sending Cardano Transaction: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
            let payload = encode $ object ["from" .= from, "to" .= to, "amount" .= amount]
            let request = setRequestHeader "project_id" [apiKey]
                          $ setRequestBodyJSON payload
                          $ parseRequest_ (apiUrl ++ "/tx/submit")
            void $ httpJSON request
