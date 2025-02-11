{-# LANGUAGE OverloadedStrings #-}

module App.Api.Controllers.AlgorandController where

import Web.Scotty
import Network.HTTP.Types (status200, status201, status404, status500)
import Infrastructure.Blockchains.Algorand as Algorand
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Web.Scotty.Swagger
import Data.Swagger

-- Swagger schema for Algorand balance response
instance ToSchema Double where
    declareNamedSchema _ = return $ NamedSchema (Just "Balance") $ mempty
        & type_ ?~ SwaggerNumber

-- Fetch Algorand account balance
getBalance :: ActionM ()
getBalance = do
    address <- param "address"
    balance <- liftIO $ Algorand.getBalance address
    case balance of
        Just b  -> status status200 >> json (object ["status" .= ("200 OK" :: String), "balance" .= b])
        Nothing -> status status404 >> json (object ["error" .= ("Address not found" :: String)])

-- Fetch Algorand transaction history (Placeholder)
getTransactions :: ActionM ()
getTransactions = do
    address <- param "address"
    status status200 >> json (object ["status" .= ("200 OK" :: String), "transactions" .= ([] :: [String])])

-- Send a transaction on Algorand blockchain
postTransaction :: ActionM ()
postTransaction = do
    from <- param "from"
    to <- param "to"
    amount <- param "amount"
    sameOwner <- param "same_owner"

    liftIO $ Algorand.sendTransaction from to amount sameOwner
    status status201 >> json (object ["message" .= ("Transaction submitted" :: String)])
