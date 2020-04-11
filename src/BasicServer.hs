{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module BasicServer where

import Control.Monad.Except (throwError)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Maybe
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment

import BasicServerTypes

type BasicAPI =
  "api" :> "version" :> Get '[JSON] Int :<|>
  "api" :> "name" :> Get '[JSON] Text :<|>
  "api" :> "description" :> Get '[JSON] Text :<|>
  "api" :> "libraries" :> Get '[JSON] Text :<|>
  "api" :> "num_users" :> Get '[JSON] Int

basicAPI :: Proxy BasicAPI
basicAPI = Proxy :: Proxy BasicAPI

versionHandler :: Handler Int
versionHandler = return 2

nameHandler :: Handler Text
nameHandler = return "Basic Server"

descriptionHandler :: Handler Text
descriptionHandler = return "A basic server we constructed using Servant"

librariesHandler :: Handler Text
librariesHandler = return "This server uses Servant"

numUsersHandler :: Handler Int
numUsersHandler = return 1

basicServer :: Server BasicAPI
basicServer =
  versionHandler :<|>
  nameHandler :<|>
  descriptionHandler :<|>
  librariesHandler :<|>
  numUsersHandler

getPort :: IO Int
getPort = do
  port <- lookupEnv "BASIC_SERVER_PORT"
  return $ read $ fromMaybe "8080" port

runServer :: IO ()
runServer = do
  port <- getPort
  run port (serve basicAPI basicServer)

nameDictionary :: Map.Map Int64 Text
nameDictionary = Map.fromList
  [ (1, "Karen")
  , (2, "Anthony")
  , (3, "Ashley")
  , (4, "Julian")
  , (5, "Christopher")
  , (6, "Amanda")
  ]
