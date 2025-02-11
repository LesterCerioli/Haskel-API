{-# LANGUAGE DeriveGeneric #-}

module Domain.Entities.Account where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.UUID (UUID)

data Account = Account
  { accountId     :: UUID
  , userId        :: UUID
  , blockchain    :: String  -- Ex: Cardano, Polkadot, etc.
  , walletAddress :: String
  , createdAt     :: String
  } deriving (Show, Generic)

instance ToJSON Account
instance FromJSON Account
