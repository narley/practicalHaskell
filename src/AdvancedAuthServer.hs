{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module AdvancedAuthServer where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.PasswordStore (verifyPassword, makePassword)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database (localConnString, runAction)
import Database.Esqueleto
import qualified Database.Persist as Persist
import Database.Persist.Postgresql (ConnectionString, Entity)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Request(..), requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client
import Servant.Client.Core hiding (Request(..), requestHeaders)
import qualified Servant.Client.Core as Client
import Servant.Server
import Servant.Server.Experimental.Auth
import qualified Web.JWT as JWT

import JWTHelpers (makeJWTCookie, decodeJWTCookie)
import Schema

newtype AdminUserId = AdminUserId Int64

type instance AuthServerData (AuthProtect "user") = Int64
type instance AuthClientData (AuthProtect "user") = Text

type instance AuthServerData (AuthProtect "admin") = AdminUserId
type instance AuthClientData (AuthProtect "admin") = Text

-- Adds an item to Request
authenticateReq :: AuthClientData (AuthProtect "user") -> AuthenticatedRequest (AuthProtect "user")
authenticateReq cookie = mkAuthenticatedRequest cookie insertHeader
  where
    insertHeader :: AuthClientData (AuthProtect "user") -> Client.Request -> Client.Request
    insertHeader cookie req = req { Client.requestHeaders =
      ("auth-token", encodeUtf8 cookie) Seq.<| Client.requestHeaders req }

adminAuthenticateReq :: AuthClientData (AuthProtect "admin") -> AuthenticatedRequest (AuthProtect "admin")
adminAuthenticateReq cookie = mkAuthenticatedRequest cookie insertHeader
  where
    insertHeader :: AuthClientData (AuthProtect "admin") -> Client.Request -> Client.Request
    insertHeader cookie req =
      req { Client.requestHeaders = ("auth-token", encodeUtf8 cookie) Seq.<| Client.requestHeaders req }

-- Retrieves item from Request
authHandler :: ConnectionString -> AuthHandler Request Int64
authHandler conn = mkAuthHandler handler
  where
    handler :: Request -> Handler Int64
    handler request = case lookup "auth-token" (requestHeaders request) of
      Nothing -> throwError (err401 { errBody = "You must log in to view this endpoint!" })
      Just cookie -> do
        let cookie' = decodeUtf8 cookie
        lookupResults <- liftIO $ runAction conn $ select . from $ \loginTokens -> do
          where_ (loginTokens ^. LoginTokenCookie ==. val cookie')
          return loginTokens
        if length lookupResults /= 1
          then throwError (err403 { errBody = "This cookie is invalid!"} )
          else do
            let uid = fromSqlKey . loginTokenUserId . entityVal . head $ lookupResults
            case decodeJWTCookie cookie' of
              Just uid -> return uid
              _ -> throwError (err403 { errBody = "This cookie is invalid"})

adminAuthHandler :: ConnectionString -> AuthHandler Request AdminUserId
adminAuthHandler conn = mkAuthHandler handler
  where
    handler :: Request -> Handler AdminUserId
    handler request = case lookup "auth-token" (requestHeaders request) of
      Nothing -> throwError (err401 { errBody = "You must log in to view this endpoint!" })
      Just cookie -> do
        let cookie' = decodeUtf8 cookie
        lookupResults <- liftIO $ runAction conn $ select . from $ \loginTokens -> do
          where_ (loginTokens ^. LoginTokenCookie ==. val cookie')
          return loginTokens
        if length lookupResults /= 1
          then throwError (err403 { errBody = "This cookie is invalid" })
          else do
            case decodeJWTCookie cookie' of
              Just uid -> return (AdminUserId uid)
              _ -> throwError (err403 { errBody = "This cookie is invalid" })

type UserAuthAPI =
  "users" :> AuthProtect "user" :> Capture "uid" Int64 :> Get '[JSON] User :<|>
  "users" :> ReqBody '[JSON] (User, Text) :> Post '[JSON] Int64 :<|>
  "users" :> "all" :> AuthProtect "admin" :> Get '[JSON] [Entity User] :<|>
  "users" :> "login" :> ReqBody '[JSON] LoginInfo :> Post '[JSON] (Int64, Text) :<|>
  "users" :> "logout" :> AuthProtect "user" :> Post '[JSON] ()

userAuthAPI :: Proxy UserAuthAPI
userAuthAPI = Proxy

-- TODO: Add user auth
fetchUserHandler :: ConnectionString -> Int64 -> Int64 -> Handler User
fetchUserHandler conn authId uid = if authId /= uid -- << Check if the IDs match after implementing auth!
  then throwError $ err403 { errBody = "Wrong User ID!"}
  else do
    results <- liftIO $ runAction conn $ get (toSqlKey uid)
    case results of
      Nothing -> throwError $ err403 { errBody = "Couldn't find this user!"}
      Just u -> return u

createHandler :: ConnectionString -> (User, Text) -> Handler Int64
createHandler conn (user, password) = liftIO $ runAction conn $ do
  userKey <- insert user
  passwordHash <- liftIO $ makePassword (encodeUtf8 password) 17
  insert (AuthData userKey passwordHash "user")
  return (fromSqlKey userKey)

-- TODO: Incorporate admin auth!
fetchAllUsersHandler :: ConnectionString -> AdminUserId -> Handler [Entity User]
fetchAllUsersHandler conn (AdminUserId adminId ) = do
  authUsers <- liftIO $ runAction conn $ select . from $ \(users `InnerJoin` authData ) -> do
    on (users ^. UserId ==. authData ^. AuthDataUserId)
    where_ (authData ^. AuthDataUserId ==. val (toSqlKey adminId))
    where_ (authData ^. AuthDataUserType ==. val (T.pack "admin"))
    return authData
  if length authUsers /= 1
    then throwError $ err401 {errBody = "You don't have permission!"}
    else
      liftIO $ runAction conn $ select . from $ \users -> return users

-- TODO
loginHandler :: ConnectionString -> LoginInfo -> Handler (Int64, Text)
loginHandler conn (LoginInfo email password) = do
  results <- liftIO $ runAction conn $ select . from $ \(users `InnerJoin` authData) -> do
    on (users ^. UserId ==. authData ^. AuthDataUserId)
    where_ (users ^. UserEmail ==. val email)
    return authData
  if length results /= 1
    then throwError $ err401 {errBody = "You don't have permission!"}
    else do
      let (Entity _ authData) = head results
      if verifyPassword (encodeUtf8 password) (authDataHashString authData)
        then do
          let cookie = makeJWTCookie (fromSqlKey $ authDataUserId authData)
          _ <- liftIO $ runAction conn $ insert (LoginToken (authDataUserId authData) cookie)
          return (fromSqlKey (authDataUserId authData), cookie)
        else throwError $ err401 {errBody = "You don't have permission!"}

-- TODO (make sure to add authentication here!)
logoutHandler :: ConnectionString -> Int64 -> Handler ()
logoutHandler conn authId = do
  liftIO $ runAction conn $ delete $ from $ \loginTokens -> do
    where_ (loginTokens ^. LoginTokenUserId ==. val (toSqlKey authId))
    return ()
  return ()

userServer :: ConnectionString -> Server UserAuthAPI
userServer conn =
  fetchUserHandler conn :<|>
  createHandler conn :<|>
  fetchAllUsersHandler conn :<|>
  loginHandler conn :<|>
  logoutHandler conn

authContext :: ConnectionString -> Context (AuthHandler Request Int64 ': AuthHandler Request AdminUserId ': '[])
authContext conn =
  authHandler conn :.
  adminAuthHandler conn :.
  EmptyContext

runServer :: IO ()
runServer = run 8080 (serveWithContext userAuthAPI (authContext localConnString) (userServer localConnString))

-- TODO Add AuthenticatedRequest parameters as needed!
-- You'll also need to add the parameters to the helper functions below!
retrieveUserClient :: AuthenticatedRequest (AuthProtect "user") -> Int64 -> ClientM User
createUserClient :: (User, Text) -> ClientM Int64
fetchAllUsersClient :: AuthenticatedRequest (AuthProtect "admin") -> ClientM [Entity User]
loginClient :: LoginInfo -> ClientM (Int64, Text)
logoutClient :: AuthenticatedRequest (AuthProtect "user") -> ClientM ()
(retrieveUserClient :<|>
 createUserClient :<|>
 fetchAllUsersClient :<|>
 loginClient :<|>
 logoutClient) = client userAuthAPI

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8080"
  return $ ClientEnv manager baseUrl Nothing

-- retrieveUserHelper :: Int64 -> Int64 -> String -> IO (Either ServantError User)
retrieveUserHelper :: Int64 -> String -> IO (Either ServantError User)
retrieveUserHelper uid cookie = do
  clientEnv' <- clientEnv
  let (cookie' :: Text) = T.pack cookie
  -- TODO Add authenticateReq parameter to call!
  runClientM (retrieveUserClient (authenticateReq cookie') uid) clientEnv'

createUserHelper :: String -> String -> Int -> String -> IO (Either ServantError Int64)
createUserHelper name email age password = do
  clientEnv' <- clientEnv
  let user = User (T.pack name) (T.pack email) age
  runClientM (createUserClient (user, T.pack password)) clientEnv'

fetchAllUsersHelper :: String -> IO (Either ServantError Int)
fetchAllUsersHelper cookie = do
  clientEnv' <- clientEnv
  let (cookie' :: Text) = T.pack cookie
  result <- runClientM (fetchAllUsersClient (adminAuthenticateReq cookie')) clientEnv'
  case result of
    (Right users) -> return $ Right (length users)
    (Left e) -> return $ Left e

loginHelper :: String -> String -> IO (Either ServantError (Int64, Text))
loginHelper email password = do
  clientEnv' <- clientEnv
  runClientM (loginClient (LoginInfo (T.pack email) (T.pack password))) clientEnv'

logoutHelper :: String -> IO (Either ServantError ())
logoutHelper cookie = do
  clientEnv' <- clientEnv
  let (cookie' :: Text) = T.pack cookie
  runClientM (logoutClient (authenticateReq cookie')) clientEnv'
