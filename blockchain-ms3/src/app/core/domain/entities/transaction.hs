{-# LANGUAGE DeriveGeneric #-}

module Domain.Entities.Transaction where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.UUID (UUID)

data Transaction = Transaction
  { transactionId :: UUID
  , accountId     :: UUID
  , blockchain    :: String
  , sender        :: String
  , receiver      :: String
  , amount        :: Double
  , currency      :: String
  , status        :: String  -- "pending", "completed", "failed"
  , createdAt     :: String
  } deriving (Show, Generic)

instance ToJSON Transaction
instance FromJSON Transaction
