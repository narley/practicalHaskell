{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module RestServer where

import Control.Monad.Except (throwError, liftIO)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server

import Schema

-- TODO: Fill in this type and the proxy!
type RestAPI =
  "api" :> "users" :> Get '[JSON] [User] :<|>
  "api" :> "users" :> Capture "id" Int64 :> Get '[JSON] User :<|>
  "api" :> "users" :> "filter" :> QueryParam "age" Int :> QueryParams "emails" Text :> Get '[JSON] [(Int64, User)] :<|>
  "api" :> "users" :> "create" :> ReqBody '[JSON] User :> Post '[JSON] Int64 :<|>
  "api" :> "users" :> "delete" :> Capture "id" Int64 :> Delete '[JSON] ()

restAPI :: Proxy RestAPI
restAPI = Proxy :: Proxy RestAPI

usersHandler :: Handler [User]
usersHandler =
  let users = snd <$> Map.toList nameDictionary
  in
    return users

userHandler :: Int64 -> Handler User
userHandler uid =
  let maybeUser = Map.lookup uid nameDictionary
  in
    case maybeUser of
      Just user -> return user
      Nothing -> throwError $ err404
        { errBody = "That user doesn't exist!" }

usersAgeEmailHandler :: Maybe Int -> [Text] -> Handler [(Int64, User)]
usersAgeEmailHandler maybeAge emails =
  let
    users = Map.toList nameDictionary
    filteredUsers = filter byEmail $ filter byAge users
  in
    return filteredUsers
  where
    byAge :: (Int64, User) -> Bool
    byAge (_, User _ _ age) =
      case maybeAge of
        Just age' -> age >= age'
        Nothing -> True
    byEmail :: (Int64, User) -> Bool
    byEmail (_, User _ email _) =
      case emails of
        xs@(_:_) -> email `elem` xs
        [] -> True

usersCreate :: User -> Handler Int64
usersCreate user =
  return 7

usersDelete :: Int64 -> Handler ()
usersDelete uid =
  let maybeUser = Map.lookup uid nameDictionary
  in
    case maybeUser of
      Just user -> do
        liftIO $ print user
        return ()
      Nothing -> throwError $ err404
        { errBody = "That user doesn't exist!"}

restServer :: Server RestAPI
restServer =
  usersHandler :<|>
  userHandler :<|>
  usersAgeEmailHandler :<|>
  usersCreate :<|>
  usersDelete

runRestAPI :: IO ()
runRestAPI = run 8080 (serve restAPI restServer)

nameDictionary :: Map.Map Int64 User
nameDictionary = Map.fromList
  [ (1, User "Karen" "karen@gmail.com" 35)
  , (2, User "Anthony" "anthony@test.com" 24)
  , (3, User "Ashley" "ashley@yahoo.com" 23)
  , (4, User "Julian" "julian@spark.com" 28)
  , (5, User "Christopher" "crv@abc.xyz" 34)
  , (6, User "Amanda" "amanda@hotmail.com" 31)
  ]
