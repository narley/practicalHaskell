{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)
import Servant.API
import Servant.Client (ClientEnv(..), parseBaseUrl, runClientM, client, ClientM, ServantError)
import Test.Hspec

import BasicServer (runServer, basicAPI)
import BasicServerTypes

main :: IO ()
main = do
  tid <- forkIO runServer
  threadDelay 1000000
  manager <- newManager tlsManagerSettings
  serverPort <- lookupEnv "BASIC_SERVER_PORT"
  baseUrl <- parseBaseUrl ("http://127.0.0.1:" ++ fromMaybe "8080" serverPort)
  let clientEnv = ClientEnv manager baseUrl Nothing
  hspec $ before (fetchTestResults clientEnv) testSpec
  hspec $ errorSpec clientEnv
  killThread tid

fetchTestResults :: ClientEnv -> IO (Either ServantError (Text, Text, DescriptionResponse))
fetchTestResults clientEnv = flip runClientM clientEnv $ do
  name1 <- nameClient (BasicServerUserId 1)
  name2 <- nameClient (BasicServerUserId 6)
  descriptionResponse <- descriptionClient
  return (name1, name2, descriptionResponse)

testSpec :: SpecWith (Either ServantError (Text, Text, DescriptionResponse))
testSpec = it "Should fetch the proper info on endpoints" $ \case
  Right (name1, name2, descriptionResponse) -> do
    name1 `shouldBe` "Karen"
    name2 `shouldBe` "Amanda"
    descriptionResponse `shouldBe` DescriptionResponse "A basic server we constructed using Servant"
  Left _ -> expectationFailure "The server calls failed!"

errorSpec :: ClientEnv -> Spec
errorSpec clientEnv = it "Should throw an error when fetching a non-existent ID" $ do
  result <- retrieveBadUser
  isLeft result `shouldBe` True
  where
    retrieveBadUser = flip runClientM clientEnv $ nameClient (BasicServerUserId 7)

nameClient :: BasicServerUserId -> ClientM Text
descriptionClient :: ClientM DescriptionResponse
(_ :<|> nameClient :<|> descriptionClient :<|> _ :<|> _) = client basicAPI
