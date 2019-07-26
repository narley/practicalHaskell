{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AuthServer where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Crypto.PasswordStore (verifyPassword, makePassword)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database (localConnString, runAction)
import Database.Esqueleto
import Database.Persist.Postgresql (ConnectionString, Entity)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client
import Servant.Server

import Schema

newtype AdminUserId = AdminUserId Int64

type UserAuthAPI =
  "users" :> Capture "uid" Int64 :> Get '[JSON] User :<|>
  "users" :> ReqBody '[JSON] (User, Text) :> Post '[JSON] Int64 :<|>
  "users" :> Get '[JSON] [Entity User]

userAuthAPI :: Proxy UserAuthAPI
userAuthAPI = Proxy

fetchUserHandler :: ConnectionString -> Int64 -> Handler User
fetchUserHandler conn uid = do
  maybeUser <- liftIO $ runAction conn $ get (toSqlKey uid)
  case maybeUser of
    Nothing -> throwError $ err404 {errBody = "This user doesn't exist!"}
    Just u -> return u

createHandler :: ConnectionString -> (User, Text) -> Handler Int64
createHandler conn (user, password) = liftIO $ runAction conn $ do
  userKey <- insert user
  passwordHash <- liftIO $ makePassword (encodeUtf8 password) 17
  insert (AuthData userKey passwordHash "user")
  return (fromSqlKey userKey)

fetchAllUsersHandler :: ConnectionString -> Handler [Entity User]
fetchAllUsersHandler conn = liftIO $ runAction conn $
  select . from $ \users -> return users

userServer :: ConnectionString -> Server UserAuthAPI
userServer conn =
  fetchUserHandler conn :<|>
  createHandler conn :<|>
  fetchAllUsersHandler conn

authCheck :: ConnectionString -> BasicAuthData -> IO (BasicAuthResult Int64)
authCheck conn (BasicAuthData email password) = undefined

adminAuthCheck :: ConnectionString -> BasicAuthData -> IO AdminUserId
adminAuthCheck conn (BasicAuthData email password) = undefined

authContext :: ConnectionString -> Context '[]
authContext conn = EmptyContext

runServer :: IO ()
runServer = run 8080 (serveWithContext userAuthAPI (authContext localConnString) (userServer localConnString))

retrieveUserClient :: Int64 -> ClientM User
createUserClient :: (User, Text) -> ClientM Int64
fetchAllUsersClient :: ClientM [Entity User]
(retrieveUserClient :<|>
 createUserClient :<|>
 fetchAllUsersClient) = client userAuthAPI

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8080"
  return $ ClientEnv manager baseUrl Nothing

retrieveUserHelper :: Int64 -> String -> String -> IO (Either ServantError User)
retrieveUserHelper uid email password = do
  clientEnv' <- clientEnv
  -- TODO: Make a BasicAuthData to pass to your endpoint!
  let (email' :: ByteString) = C.pack email
  let (password' :: ByteString) = C.pack password
  runClientM (retrieveUserClient uid) clientEnv'

createUserHelper :: String -> String -> Int -> String -> IO (Either ServantError Int64)
createUserHelper name email age password = do
  clientEnv' <- clientEnv
  let user = User (T.pack name) (T.pack email) age
  runClientM (createUserClient (user, T.pack password)) clientEnv'

fetchAllUsersHelper :: String -> String -> IO (Either ServantError Int)
fetchAllUsersHelper email password = do
  clientEnv' <- clientEnv
  let (email' :: ByteString) = C.pack email
  let (password' :: ByteString) = C.pack password
  result <- runClientM fetchAllUsersClient clientEnv'
  case result of
    (Right users) -> return $ Right (length users)
    (Left e) -> return $ Left e
