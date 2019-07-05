module RestAPIClient where

import Data.Int (Int64)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client

import RestServer

-- TODO: Define your client functions! They should be called:
-- getAllClient
-- getUserClient
-- filterUsersClient
-- createUserClient
-- deleteuserClient

runClientAction :: ClientM a -> IO a
runClientAction action = undefined

-- TODO: Make an API type for the Secret API!
-- type SecretAPI = ...

retrieveSecret :: IO Text
retrieveSecret = undefined