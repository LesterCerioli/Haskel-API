module Infrastructure.Blockchains.Polkadot where

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

-- Function to get Polkadot RPC API URL based on selected environment
getPolkadotRpcUrl :: IO String
getPolkadotRpcUrl = do
    env <- getEnvVariable "APP_ENV" -- "development" or "production"
    if env == "production"
        then getEnvVariable "POLKADOT_RPC_URL_PROD"
        else getEnvVariable "POLKADOT_RPC_URL_DEV"

-- Function to get Polkadot API Key based on selected environment (if required)
getPolkadotApiKey :: IO String
getPolkadotApiKey = do
    env <- getEnvVariable "APP_ENV"
    if env == "production"
        then getEnvVariable "POLKADOT_API_KEY_PROD"
        else getEnvVariable "POLKADOT_API_KEY_DEV"

-- Function to check balance on the Polkadot blockchain
getBalance :: String -> IO (Maybe Double)
getBalance address = do
    rpcUrl <- getPolkadotRpcUrl
    apiKey <- getPolkadotApiKey
    let payload = encode $ object ["address" .= address]
    let request = setRequestHeader "Authorization" [apiKey] -- If authentication is required
                  $ setRequestBodyJSON payload
                  $ parseRequest_ (rpcUrl ++ "/account/balance")
    response <- httpJSON request
    return $ decode (getResponseBody response)

-- Function to send transactions on the Polkadot blockchain
sendTransaction :: String -> String -> Double -> Bool -> IO ()
sendTransaction from to amount sameOwner = do
    rpcUrl <- getPolkadotRpcUrl
    apiKey <- getPolkadotApiKey
    if sameOwner
        then putStrLn $ "Internal Polkadot Transfer: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
        else do
            putStrLn $ "Sending Polkadot Transaction: " ++ from ++ " → " ++ to ++ " | Amount: " ++ show amount
            let payload = encode $ object ["from" .= from, "to" .= to, "amount" .= amount]
            let request = setRequestHeader "Authorization" [apiKey] -- If authentication is required
                          $ setRequestBodyJSON payload
                          $ parseRequest_ (rpcUrl ++ "/tx/send")
            void $ httpJSON request
