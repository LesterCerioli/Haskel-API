{-# LANGUAGE OverloadedStrings #-}

module Domain.Interfaces.TransactionRepositoryContract where

import Domain.Entities.Transaction
import Data.UUID (UUID)
import Control.Monad.IO.Class (MonadIO)

class MonadIO m => TransactionRepositoryContract m where
    createTransaction :: Transaction -> m ()
    getTransactionById :: UUID -> m (Maybe Transaction)
    getTransactionsByAccountId :: UUID -> m [Transaction]
    updateTransactionStatus :: UUID -> String -> m ()
