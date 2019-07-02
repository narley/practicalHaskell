{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)
import Servant.API
import Servant.Client (ClientEnv(..), parseBaseUrl, runClientM, client, ClientM)
import Test.Hspec

import BasicServer (runServer, basicAPI)

main :: IO ()
main = do
  tid <- forkIO runServer
  threadDelay 1000000
  manager <- newManager tlsManagerSettings
  serverPort <- lookupEnv "BASIC_SERVER_PORT"
  baseUrl <- parseBaseUrl ("http://127.0.0.1:" ++ fromMaybe "8080" serverPort)
  let clientEnv = ClientEnv manager baseUrl Nothing
  hspec $ envVarSpec serverPort
  hspec $ before (fetchNewItems clientEnv) testSpec
  killThread tid

envVarSpec :: Maybe String -> Spec
envVarSpec serverPort = it "Should fetch the basic server port environment variable" $
  serverPort `shouldBe` Just "8080"

fetchNewItems :: ClientEnv -> IO (Text, Int)
fetchNewItems clientEnv = flip runClientM clientEnv $ do
  librariesResponse <- fetchLibrariesClient
  numUsersResponse <- fetchNumUsersClient
  return (librariesResponse, numUsersResponse)

testSpec :: SpecWith (Text, Int)
testSpec = it "Should fetch the proper info on new endpoints" $ \(libraries, numUsers) -> do
  libraries `shouldBe` "This server uses Servant"
  numUsers `shouldBe` 1

fetchLibrariesClient :: ClientM Text
fetchNumUsersClient :: ClientM Int
(_ :<|> _ :<|> _ :<|> fetchLibrariesClient :<|> fetchNumUsersClient) = client basicAPI
