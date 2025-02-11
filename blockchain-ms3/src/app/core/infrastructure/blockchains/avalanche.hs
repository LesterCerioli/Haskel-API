module Infrastructure.Blockchains.Avalanche where

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

-- Function to get Avalanche API URL based on selected environment
getAvalancheApiUrl :: IO String
getAvalancheApiUrl = do
    env <- getEnvVariable "APP_ENV" -- "development" or "production"
    if env == "production"
        then getEnvVariable "AVALANCHE_API_URL_PROD"
        else getEnvVariable "AVALANCHE_API_URL_DEV"

-- Function to get Avalanche API Key based on selected environment
getAvalancheApiKey :: IO String
getAvalancheApiKey = do
    env <- getEnvVariable "APP_ENV"
    if env == "production"
        then getEnvVariable "AVALANCHE_API_KEY_PROD"
        else getEnvVariable "AVALANCHE_API_KEY_DEV"

-- Function to get AVAX balance from an Avalanche address
getBalance :: String -> IO (Maybe Double)
getBalance address = do
    apiUrl <- getAvalancheApiUrl
    apiKey <- getAvalancheApiKey
    let payload = encode $ object ["jsonrpc" .= ("2.0" :: String),
                                   "id" .= (1 :: Int),
                                   "method" .= ("eth_getBalance" :: String),
                                   "params" .= [address, "latest"]]
    let request = setRequestHeader "X-API-Key" [apiKey]
                  $ setRequestBodyJSON payload
                  $ parseRequest_ apiUrl
    response <- httpJSON request
    return $ decode (getResponseBody response)

-- Function to send AVAX between accounts (internal or external)
sendTransaction :: String -> String -> Double -> Bool -> IO ()
sendTransaction from to amount sameOwner = do
    apiUrl <- getAvalancheApiUrl
    apiKey <- getAvalancheApiKey
    if sameOwner
        then putStrLn $ "🔄 Internal Avalanche Transfer: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
        else do
            putStrLn $ "🚀 Sending Avalanche Transaction: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
            let payload = encode $ object ["jsonrpc" .= ("2.0" :: String),
                                           "id" .= (1 :: Int),
                                           "method" .= ("eth_sendTransaction" :: String),
                                           "params" .= [object ["from" .= from, "to" .= to, "value" .= amount]]]
            let request = setRequestHeader "X-API-Key" [apiKey]
                          $ setRequestBodyJSON payload
                          $ parseRequest_ apiUrl
            void $ httpJSON request
