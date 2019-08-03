{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module FinalServer where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Crypto.PasswordStore (verifyPassword, makePassword)
import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.Esqueleto
import Database.Persist.Postgresql (ConnectionString, Entity(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client
import Servant.Server
import System.Environment (lookupEnv)

import Database (localConnString, runAction)
import Schema

type BasicAPI =
  "hello" :> Get '[JSON] Text :<|>
  "create" :> ReqBody '[JSON] (User, Text) :> Post '[JSON] Int64 :<|>
  "retrieve" :> BasicAuth "user" Int64 :> Capture "uid" Int64 :> Get '[JSON] User

basicAPI :: Proxy BasicAPI
basicAPI = Proxy :: Proxy BasicAPI

helloHandler :: Handler Text
helloHandler = return "Hello Haskell Heroku"

createHandler :: ConnectionString -> (User, Text) -> Handler Int64
createHandler conn (user, password) = liftIO $ runAction conn $ do
  userKey <- insert user
  passwordHash <- liftIO $ makePassword (encodeUtf8 password) 17
  insert (AuthData userKey passwordHash "user")
  return (fromSqlKey userKey)

retrieveHandler :: ConnectionString -> Int64 -> Int64 -> Handler User
retrieveHandler conn authId uid = if authId /= uid
  then throwError $ err403 { errBody = "This is not the correct user!" }
  else do
    maybeUser <- liftIO $ runAction conn $ get (toSqlKey uid)
    case maybeUser of
      Nothing -> throwError $ err404 { errBody = "This user doesn't exist!" }
      Just u -> return u

basicServer :: ConnectionString -> Server BasicAPI
basicServer conn =
  helloHandler :<|>
  createHandler conn :<|>
  retrieveHandler conn

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

authContext :: ConnectionString -> Context (BasicAuthCheck Int64 ': '[])
authContext conn = BasicAuthCheck (authCheck conn) :. EmptyContext

runServer :: IO ()
runServer = do
  portString <- lookupEnv "PORT"
  let portNum = case portString of
        Nothing -> 8080
        Just s -> read s
  databaseString <- lookupEnv "DATABASE_URL"
  let conn = case databaseString of
        Nothing -> localConnString
        Just s -> C.pack s
  run portNum (serveWithContext basicAPI (authContext conn) (basicServer conn))

helloClient :: ClientM Text
(helloClient :<|> _ :<|> _) = client basicAPI

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8080"
  return $ ClientEnv manager baseUrl Nothing

helloHelper :: IO (Either ServantError Text)
helloHelper = do
  clientEnv' <- clientEnv
  runClientM helloClient clientEnv'
