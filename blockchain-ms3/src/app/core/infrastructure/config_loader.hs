{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.ConfigLoader where

import Data.Yaml (decodeFileThrow)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Directory (doesFileExist)
import Control.Monad (when)

data DBConfig = DBConfig
  { dbHost :: String
  , dbPort :: Int
  , dbUser :: String
  , dbPassword :: String
  , dbName :: String
  } deriving (Show)

data ESConfig = ESConfig
  { esHost :: String
  , esIndex :: String
  } deriving (Show)

data AppConfig = AppConfig
  { database :: DBConfig
  , elasticsearch :: ESConfig
  } deriving (Show)

loadAppConfig :: IO AppConfig
loadAppConfig = do
  env <- fromMaybe "development" <$> lookupEnv "APP_ENV"

  let envFile = if env == "production" then ".env.production" else ".env.development"

  -- Load environment variables from  specific file
  envExists <- doesFileExist envFile
  when envExists (loadEnvFile envFile)

  dbConfig <- loadDBConfig
  esConfig <- loadESConfig

  return AppConfig { database = dbConfig, elasticsearch = esConfig }

loadDBConfig :: IO DBConfig
loadDBConfig = do
  dbHost <- fromMaybe "localhost" <$> lookupEnv "POSTGRES_HOST"
  dbPort <- fromMaybe "5432" <$> lookupEnv "POSTGRES_PORT"
  dbUser <- fromMaybe "user" <$> lookupEnv "POSTGRES_USER"
  dbPassword <- fromMaybe "password" <$> lookupEnv "POSTGRES_PASSWORD"
  dbName <- fromMaybe "database" <$> lookupEnv "POSTGRES_DB"

  return DBConfig
    { dbHost = dbHost
    , dbPort = read dbPort
    , dbUser = dbUser
    , dbPassword = dbPassword
    , dbName = dbName
    }

loadESConfig :: IO ESConfig
loadESConfig = do
  esHost <- fromMaybe "http://localhost:9200" <$> lookupEnv "ELASTICSEARCH_HOST"
  esIndex <- fromMaybe "transactions" <$> lookupEnv "ELASTICSEARCH_INDEX"

  return ESConfig
    { esHost = esHost
    , esIndex = esIndex
    }

loadEnvFile :: FilePath -> IO ()
loadEnvFile file = do
  content <- readFile file
  let envVars = map (break (== '=')) (lines content)
  mapM_ (\(key, value) -> setEnv key (drop 1 value)) envVars
