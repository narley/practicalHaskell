{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module RestServer where

import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server

import Schema

-- TODO: Fill in this type and the proxy!
-- type RestAPI = ...
-- restAPI :: Proxy RestAPI

runRestAPI :: IO ()
runRestAPI = undefined

nameDictionary :: Map.Map Int64 User
nameDictionary = Map.fromList
  [ (1, User "Karen" "karen@gmail.com" 35)
  , (2, User "Anthony" "anthony@test.com" 24)
  , (3, User "Ashley" "ashley@yahoo.com" 23)
  , (4, User "Julian" "julian@spark.com" 28)
  , (5, User "Christopher" "crv@abc.xyz" 34)
  , (6, User "Amanda" "amanda@hotmail.com" 31)
  ]
