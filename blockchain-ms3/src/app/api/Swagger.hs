module App.Api.Swagger where

import Web.Scotty
import Web.Scotty.Swagger
import Data.Swagger
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

-- Define Swagger metadata
swaggerDoc :: Swagger
swaggerDoc = mempty
    { _swaggerInfo = mempty
        { _infoTitle = "Blockchain API"
        , _infoDescription = Just "API to integrate with blockchains, including Algorand, Polkadot, Avalanche and anothers."
        , _infoVersion = "1.0"
        }
    }

-- Serve Swagger UI at /docs
serveSwagger :: ScottyM ()
serveSwagger = swagger "docs" swaggerDoc
