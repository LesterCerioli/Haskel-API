{-# LANGUAGE OverloadedStrings #-}

module Domain.Interfaces.BlockchainRepositoryContract where

import Domain.Entities.Blockchain
import Control.Monad.IO.Class (MonadIO)

class MonadIO m => BlockchainRepositoryContract m where
    getSupportedBlockchains :: m [Blockchain]
    getBlockchainByName :: String -> m (Maybe Blockchain)
