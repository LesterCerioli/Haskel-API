module Main (main) where

import Web.Scotty
import App.Api.Routes (routes)

import Lib

main = do
    putStrLn "🚀 Starting Blockchain API Service with Swagger at /docs..."
    scotty 3000 routes
