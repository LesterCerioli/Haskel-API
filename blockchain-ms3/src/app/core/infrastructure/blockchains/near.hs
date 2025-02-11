module Infrastructure.Blockchains.Near where

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

-- Function to get NEAR RPC API URL based on selected environment
getNearRpcUrl :: IO String
getNearRpcUrl = do
    env <- getEnvVariable "APP_ENV" -- "development" or "production"
    if env == "production"
        then getEnvVariable "NEAR_RPC_URL_PROD"
        else getEnvVariable "NEAR_RPC_URL_DEV"

-- Function to get NEAR API Key based on selected environment (if required)
getNearApiKey :: IO String
getNearApiKey = do
    env <- getEnvVariable "APP_ENV"
    if env == "production"
        then getEnvVariable "NEAR_API_KEY_PROD"
        else getEnvVariable "NEAR_API_KEY_DEV"

-- Function to check the balance on the NEAR blockchain
getBalance :: String -> IO (Maybe Double)
getBalance address = do
    rpcUrl <- getNearRpcUrl
    apiKey <- getNearApiKey
    let payload = encode $ object ["method" .= ("query" :: String),
                                   "params" .= object ["request_type" .= ("view_account" :: String),
                                                       "finality" .= ("final" :: String),
                                                       "account_id" .= address]]
    let request = setRequestHeader "Authorization" [apiKey] -- If authentication is required
                  $ setRequestBodyJSON payload
                  $ parseRequest_ (rpcUrl ++ "/query")
    response <- httpJSON request
    return $ decode (getResponseBody response)

-- Function to send transactions on the NEAR blockchain
sendTransaction :: String -> String -> Double -> Bool -> IO ()
sendTransaction from to amount sameOwner = do
    rpcUrl <- getNearRpcUrl
    apiKey <- getNearApiKey
    if sameOwner
        then putStrLn $ "🔄 Internal NEAR Transfer: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
        else do
            putStrLn $ "🚀 Sending NEAR Transaction: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
            let payload = encode $ object ["from" .= from, "to" .= to, "amount" .= amount]
            let request = setRequestHeader "Authorization" [apiKey] -- If authentication is required
                          $ setRequestBodyJSON payload
                          $ parseRequest_ (rpcUrl ++ "/tx/send")
            void $ httpJSON request
