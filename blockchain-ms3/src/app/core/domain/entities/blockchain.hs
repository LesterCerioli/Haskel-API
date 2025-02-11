{-# LANGUAGE DeriveGeneric #-}

module Domain.Entities.Blockchain where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data Blockchain = Blockchain
  { blockchainName :: String
  , network        :: String  -- "mainnet" ou "testnet"
  , explorerUrl    :: String
  } deriving (Show, Generic)

instance ToJSON Blockchain
instance FromJSON Blockchain
