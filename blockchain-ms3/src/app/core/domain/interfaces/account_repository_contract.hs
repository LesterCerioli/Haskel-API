{-# LANGUAGE OverloadedStrings #-}

module Domain.Interfaces.AccountRepositoryContract where

import Domain.Entities.Account
import Data.UUID (UUID)
import Control.Monad.IO.Class (MonadIO)

class MonadIO m => AccountRepositoryContract m where
    createAccount :: Account -> m ()
    getAccountById :: UUID -> m (Maybe Account)
    getAccountsByUserId :: UUID -> m [Account]
    deleteAccount :: UUID -> m ()
