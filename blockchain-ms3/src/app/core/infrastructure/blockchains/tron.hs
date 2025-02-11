module Infrastructure.Blockchains.Tron where

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

-- Function to get Tron RPC API URL based on selected environment
getTronRpcUrl :: IO String
getTronRpcUrl = do
    env <- getEnvVariable "APP_ENV" -- "development" or "production"
    if env == "production"
        then getEnvVariable "TRON_RPC_URL_PROD"
        else getEnvVariable "TRON_RPC_URL_DEV"

-- Function to get Tron API Key based on selected environment (if required)
getTronApiKey :: IO String
getTronApiKey = do
    env <- getEnvVariable "APP_ENV"
    if env == "production"
        then getEnvVariable "TRON_API_KEY_PROD"
        else getEnvVariable "TRON_API_KEY_DEV"

-- Function to check balance on the Tron blockchain
getBalance :: String -> IO (Maybe Double)
getBalance address = do
    rpcUrl <- getTronRpcUrl
    apiKey <- getTronApiKey
    let request = setRequestHeader "Authorization" [apiKey] -- If authentication is required
                  $ parseRequest_ (rpcUrl ++ "/wallet/getaccount?address=" ++ address)
    response <- httpJSON request
    return $ decode (getResponseBody response)

-- Function to send transactions on the Tron blockchain
sendTransaction :: String -> String -> Double -> Bool -> IO ()
sendTransaction from to amount sameOwner = do
    rpcUrl <- getTronRpcUrl
    apiKey <- getTronApiKey
    if sameOwner
        then putStrLn $ "Internal Tron Transfer: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
        else do
            putStrLn $ "Sending Tron Transaction: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
            let payload = encode $ object ["from" .= from, "to" .= to, "amount" .= amount]
            let request = setRequestHeader "Authorization" [apiKey] -- If authentication is required
                          $ setRequestBodyJSON payload
                          $ parseRequest_ (rpcUrl ++ "/wallet/sendtransaction")
            void $ httpJSON request
