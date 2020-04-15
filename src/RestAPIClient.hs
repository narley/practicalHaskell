{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module RestAPIClient where

import Data.Aeson
import Data.Int (Int64)
import Data.Proxy
import Data.Text (Text, pack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

import Schema
import RestServer

import Data.Either

-- TODO: Define your client functions! They should be called:
getAllClient :: ClientM [User]
getUserClient :: Int64 -> ClientM User
filterUsersClient :: Maybe Int -> [Text] -> ClientM [(Int64, User)]
createUserClient :: User -> ClientM Int64
deleteUserClient :: Int64 -> ClientM ()

(getAllClient :<|>
 getUserClient :<|>
 filterUsersClient :<|>
 createUserClient :<|>
 deleteUserClient) = client restAPI

runClientAction :: Int -> ClientM a -> IO (Either ServantError a)
runClientAction portNum action = do
  manager' <- newManager tlsManagerSettings
  baseUrl' <- parseBaseUrl ("127.0.0.1:" <> show portNum)
  let clientEnv = mkClientEnv manager' baseUrl'
  runClientM action clientEnv


-- TODO: Make an API type for the Secret API!
type SecretAPI =
  "auth" :> ReqBody '[JSON] LoginInfo :> Post '[JSON] Text :<|>
  "secret" :> Capture "api_key" Text :> Get '[JSON] Text

secretAPI :: Proxy SecretAPI
secretAPI = Proxy :: Proxy SecretAPI

getApiKeyClient :: LoginInfo -> ClientM Text
getSecretClient :: Text -> ClientM Text
(getApiKeyClient :<|>
 getSecretClient) = client secretAPI

retrieveSecret :: IO Text
retrieveSecret = do
  apiKey <- runClientAction 8081 $ getApiKeyClient login
  secret <- runClientAction 8081 $ getSecretClient $ fromRight (pack "") apiKey
  case secret of
    Right secret' -> return secret'
    Left _ -> mempty
  where
    login = LoginInfo (pack "test") (pack "pass")
