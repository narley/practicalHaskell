{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module BasicServer where

import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server

type BasicAPI =
  "api" :> "version" :> Get '[JSON] Int :<|>
  "api" :> "name" :> Get '[JSON] Text :<|>
  "api" :> "description" :> Get '[JSON] Text

basicAPI :: Proxy BasicAPI
basicAPI = Proxy :: Proxy BasicAPI

versionHandler :: Handler Int
versionHandler = return 2

nameHandler :: Handler Text
nameHandler = return "Basic Server"

descriptionHandler :: Handler Text
descriptionHandler = return "A basic server we constructed using Servant"

basicServer :: Server BasicAPI
basicServer =
  versionHandler :<|>
  nameHandler :<|>
  descriptionHandler

runServer :: IO ()
runServer = run 8080 (serve basicAPI basicServer)

nameDictionary :: Map.Map Int64 Text
nameDictionary = Map.fromList
  [ (1, "Karen")
  , (2, "Anthony")
  , (3, "Ashley")
  , (4, "Julian")
  , (5, "Christopher")
  , (6, "Amanda")
  ]