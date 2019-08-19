{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module RestAPIClient where

import Data.Aeson
import Data.Int (Int64)
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

import Schema
import RestServer

-- TODO: Define your client functions! They should be called:
-- getAllClient
-- getUserClient
-- filterUsersClient
-- createUserClient
-- deleteuserClient
runClientAction :: Int -> ClientM a -> IO (Either ServantError a)
runClientAction portNum action = undefined


-- TODO: Make an API type for the Secret API!
-- type SecretAPI = ...

retrieveSecret :: IO Text
retrieveSecret = undefined