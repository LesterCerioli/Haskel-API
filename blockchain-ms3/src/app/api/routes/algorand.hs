{-# LANGUAGE OverloadedStrings #-}

module App.Api.Routes.Algorand where

import Web.Scotty
import App.Api.Controllers.AlgorandController (getBalance, getTransactions, postTransaction)
import Web.Scotty.Swagger
import Data.Swagger

-- Define Algorand API routes with Swagger annotations
algorandRoutes :: ScottyM ()
algorandRoutes = do
    swaggerRoute "GET" "/algorand/balance/:address" "Fetch Algorand balance" getBalance
    swaggerRoute "GET" "/algorand/transactions/:address" "Fetch Algorand transactions" getTransactions
    swaggerRoute "POST" "/algorand/transactions" "Send Algorand transaction" postTransaction
