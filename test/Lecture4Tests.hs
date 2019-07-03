{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client (ClientEnv(..), parseBaseUrl, runClientM, client, ClientM)
import Test.Hspec

import Schema
import RestServer

data TestResults = TestResults
  { getAllResults :: [User]
  , getIndividualResults :: (User, User)
  , getFilterResults :: ([(Int64, User)], [(Int64, User)], [(Int64, User)], [(Int64, User)])
  , postUserResults :: Int64
  }

main :: IO ()
main = do
  tid <- forkIO runRestAPI
  threadDelay 1000000
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8080"
  let clientEnv = ClientEnv manager baseUrl Nothing
  hspec $ before (fetchTestResults clientEnv) testSpec
  hspec $ errorSpec clientEnv
  killThread tid

fetchTestResults :: ClientEnv -> IO TestResults
fetchTestResults clientEnv = flip runClientM clientEnv $ do
  allUsers <- getAllClient
  user1 <- getIndividualClient 2
  user2 <- getIndividualClient 6
  filterResult1 <- filterClient Nothing ["anthony@test.com", "julian@spark.com"]
  filterResult2 <- filterClient (Just 26) ["anthony@test.com", "julian@spark.com"]
  filterResult3 <- filterClient (Just 26) []
  filterResult4 <- filterClient (Just 45) ["anthony@test.com", "julian@spark.com"]
  postResult <- postClient (User "James" "james@test.com" 28)
  return $ TestResults
    allUsers
    (user1, user2)
    (filterResult1, filterResult2, filterResult3, filterResult4)
    postResult

testSpec :: SpecWith TestResults
testSpec = it "Should fetch the proper info on endpoints" $ \testResults -> do
  getAllResults testResults `shouldBe` snd <$> Map.toList nameDictionary
  (fst . getIndividualResults $ testResults) `shouldBe` User "Anthony" "anthony@test.com" 24
  (snd . getIndividualResults $ testResults) `shouldBe` User "Amanda" "amanda@hotmail.com" 31
  getFilterResults testResults `shouldBe`
    ( [(2, User "Anthony" "anthony@test.com" 24), (4, User "Julian" "julian@spark.com" 28)]
    , [(4, User "Julian" "julian@spark.com" 28)]
    , [(1, User "Karen" "karen@gmail.com" 35), (4, User "Julian" "julian@spark.com" 28), (5, User "Christopher" "crv@abc.xyz" 34), (6, User "Amanda" "amanda@hotmail.com" 31)]
    , []
    )
  postUserResults testResults `shouldBe` 7

errorSpec :: ClientEnv -> Spec
errorSpec clientEnv = it "Should throw an error when fetching a non-existent ID" $ do
  retrieveBadUser `shouldThrow` anyErrorCall
  deleteBadUser `shouldThrow` anyErrorCall
  where
    retrieveBadUser = flip runClientM clientEnv $ getIndividualClient 8
    deleteBadUser = flip runClientM clientEnv $ deleteClient 10

getAllClient :: ClientM [User]
getIndividualClient :: Int64 -> ClientM User
filterClient :: Maybe Int -> [Text] -> ClientM [(Int64, User)]
postClient :: User -> ClientM Int64
deleteClient :: Int64 -> ClientM ()
(getAllClient :<|>
 getIndividualClient :<|>
 filterClient :<|>
 postClient :<|>
 deleteClient) = client restAPI
