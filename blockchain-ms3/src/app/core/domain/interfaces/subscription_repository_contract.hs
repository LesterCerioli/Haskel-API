{-# LANGUAGE OverloadedStrings #-}

module Domain.Interfaces.SubscriptionRepositoryContract where

import Domain.Entities.Subscription
import Data.UUID (UUID)
import Control.Monad.IO.Class (MonadIO)

class MonadIO m => SubscriptionRepositoryContract m where
    createSubscription :: Subscription -> m ()
    getSubscriptionByUserId :: UUID -> m (Maybe Subscription)
    updateSubscriptionStatus :: UUID -> String -> m ()
