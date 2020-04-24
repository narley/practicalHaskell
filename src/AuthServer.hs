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
  "users" :> BasicAuth "user" Int64 :> Capture "uid" Int64 :> Get '[JSON] User :<|>
  "users" :> "create" :> ReqBody '[JSON] (User, Text) :> Post '[JSON] Int64 :<|>
  "users" :> "all" :> BasicAuth "admin" AdminUserId :> Get '[JSON] [Entity User] :<|>
  -- This breaks unit tests
  "users" :> "create" :> "admin" :> BasicAuth "admin" AdminUserId :> ReqBody '[JSON] (User, Text) :> Post '[JSON] Int64

userAuthAPI :: Proxy UserAuthAPI
userAuthAPI = Proxy

fetchUserHandler :: ConnectionString -> Int64 -> Int64 -> Handler User
fetchUserHandler conn authId uid =
  if authId == uid then do
    maybeUser <- liftIO $ runAction conn $ get (toSqlKey uid)
    case maybeUser of
      Nothing -> throwError $ err404 {errBody = "This user doesn't exist!"}
      Just u -> return u
  else
    throwError $ err401 {errBody = "You don't have permission!"}

createHandler :: ConnectionString -> (User, Text) -> Handler Int64
createHandler conn (user, password) = liftIO $ runAction conn $ do
  userKey <- insert user
  passwordHash <- liftIO $ makePassword (encodeUtf8 password) 17
  insert (AuthData userKey passwordHash "user")
  return (fromSqlKey userKey)

fetchAllUsersHandler :: ConnectionString -> AdminUserId -> Handler [Entity User]
fetchAllUsersHandler conn _ = liftIO $ runAction conn $
  select . from $ \users ->
    return users

-- This breaks unit tests
createAdminHanler :: ConnectionString -> AdminUserId -> (User, Text) -> Handler Int64
createAdminHanler conn (AdminUserId authId) (user, password) =  do
  maybeUser <- liftIO $ runAction conn $ get (toSqlKey authId)
  case (maybeUser :: Maybe User) of
    Nothing -> throwError $ err404 {errBody = "This user doesn't exist!"}
    Just _ -> do
        userKey <- liftIO $ runAction conn $ insert user
        passwordHash <- liftIO $ makePassword (encodeUtf8 password) 17
        _ <- liftIO $ runAction conn $ insert (AuthData userKey passwordHash "admin")
        return (fromSqlKey userKey)

userServer :: ConnectionString -> Server UserAuthAPI
userServer conn =
  fetchUserHandler conn :<|>
  createHandler conn :<|>
  fetchAllUsersHandler conn :<|>
  -- This breaks unit tests
  createAdminHanler conn

authCheck :: ConnectionString -> BasicAuthData -> IO (BasicAuthResult Int64)
authCheck conn (BasicAuthData email password) = do
  results <- runAction conn $ select . from $ \(users `InnerJoin` authData) -> do
    on (users ^. UserId ==. authData ^. AuthDataUserId)
    where_ (users ^. UserEmail ==. val (decodeUtf8 email))
    return authData
  if length results /= 1
    then return NoSuchUser
    else do
      let (Entity _ authData) = head results
      if verifyPassword password (authDataHashString authData)
        then return $ Authorized (fromSqlKey (authDataUserId authData))
        else return BadPassword

adminAuthCheck :: ConnectionString -> BasicAuthData -> IO (BasicAuthResult AdminUserId)
adminAuthCheck conn (BasicAuthData email password) = do
  results <- runAction conn $ select . from $ \(users `InnerJoin` authData) -> do
    on (users ^. UserId ==. authData ^. AuthDataUserId)
    where_ (users ^. UserEmail ==. val (decodeUtf8 email))
    where_ (authData ^. AuthDataUserType ==. val "admin")
    return authData
  if length results /= 1
    then return NoSuchUser
    else do
      let (Entity _ authData) = head results
      if verifyPassword password (authDataHashString authData)
        then return $ Authorized (AdminUserId $ fromSqlKey (authDataUserId authData))
        else return BadPassword

authContext :: ConnectionString -> Context (BasicAuthCheck Int64 ': BasicAuthCheck  AdminUserId ': '[])
authContext conn =
  BasicAuthCheck (authCheck conn) :.
  BasicAuthCheck (adminAuthCheck conn) :.
  EmptyContext

runServer :: IO ()
runServer = run 8080 (serveWithContext userAuthAPI (authContext localConnString) (userServer localConnString))

retrieveUserClient :: BasicAuthData -> Int64 -> ClientM User
createUserClient :: (User, Text) -> ClientM Int64
fetchAllUsersClient :: BasicAuthData -> ClientM [Entity User]
-- This breaks unit tests
createAdminClient :: BasicAuthData -> (User, Text) -> ClientM Int64
(retrieveUserClient :<|>
 createUserClient :<|>
 fetchAllUsersClient :<|>
-- This breaks unit tests
 createAdminClient) = client userAuthAPI

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8080"
  return $ ClientEnv manager baseUrl Nothing

retrieveUserHelper :: Int64 -> String -> String -> IO (Either ServantError User)
retrieveUserHelper uid email password = do
  clientEnv' <- clientEnv
  let (email' :: ByteString) = C.pack email
  let (password' :: ByteString) = C.pack password
  runClientM (retrieveUserClient (BasicAuthData email' password') uid) clientEnv'

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
  result <- runClientM (fetchAllUsersClient (BasicAuthData email' password')) clientEnv'
  case result of
    (Right users) -> return $ Right (length users)
    (Left e) -> return $ Left e
