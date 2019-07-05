{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Either (isLeft)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client (ClientEnv(..), parseBaseUrl, runClientM, client, ClientM, ServantError)
import Test.Hspec

import Schema
import RestServer

data TestResults = TestResults
  { getAllResults :: Either ServantError [User]
  , getIndividualResults :: (Either ServantError User, Either ServantError User)
  , getFilterResults ::
      ( Either ServantError [(Int64, User)]
      , Either ServantError [(Int64, User)]
      , Either ServantError [(Int64, User)]
      , Either ServantError [(Int64, User)]
      )
  , postUserResults :: Either ServantError Int64
  , badRetrieveResult :: Either ServantError User
  , badDeleteResult :: Either ServantError ()
  }

main :: IO ()
main = do
  tid <- forkIO runRestAPI
  threadDelay 1000000
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8080"
  let clientEnv = ClientEnv manager baseUrl Nothing
  hspec $ beforeAll (fetchTestResults clientEnv) testSpec
  killThread tid

fetchTestResults :: ClientEnv -> IO TestResults
fetchTestResults clientEnv = do
  allUsers <- runClientM getAllClient clientEnv
  user1 <- runClientM (getIndividualClient 2) clientEnv
  user2 <- runClientM (getIndividualClient 6) clientEnv
  filterResult1 <- runClientM (filterClient Nothing ["anthony@test.com", "julian@spark.com"]) clientEnv
  filterResult2 <- runClientM (filterClient (Just 26) ["anthony@test.com", "julian@spark.com"]) clientEnv
  filterResult3 <- runClientM (filterClient (Just 26) []) clientEnv
  filterResult4 <- runClientM (filterClient (Just 45) ["anthony@test.com", "julian@spark.com"]) clientEnv
  postResult <- runClientM (postClient (User "James" "james@test.com" 28)) clientEnv
  badRetrieve <- runClientM (getIndividualClient 8) clientEnv
  badDelete <- runClientM (deleteClient 10) clientEnv
  return $ TestResults
    allUsers
    (user1, user2)
    (filterResult1, filterResult2, filterResult3, filterResult4)
    postResult
    badRetrieve
    badDelete

testSpec :: SpecWith TestResults
testSpec = describe "Running client functions gives the correct results" $ do
  it "Should retrieve all users properly" $ \testResults ->
    getAllResults testResults `shouldBe` Right (snd <$> Map.toList nameDictionary)
  it "Should retrieve individual users properly" $ \testResults -> do
    (fst . getIndividualResults $ testResults) `shouldBe` (Right $ User "Anthony" "anthony@test.com" 24)
    (snd . getIndividualResults $ testResults) `shouldBe` (Right $ User "Amanda" "amanda@hotmail.com" 31)
  it "Should retrieve filtered users properly" $ \testResults ->
    getFilterResults testResults `shouldBe`
      ( Right [(2, User "Anthony" "anthony@test.com" 24), (4, User "Julian" "julian@spark.com" 28)]
      , Right [(4, User "Julian" "julian@spark.com" 28)]
      , Right [(1, User "Karen" "karen@gmail.com" 35), (4, User "Julian" "julian@spark.com" 28), (5, User "Christopher" "crv@abc.xyz" 34), (6, User "Amanda" "amanda@hotmail.com" 31)]
      , Right []
      )
  it "Should post a new user correctly" $ \testResults ->
    postUserResults testResults `shouldBe` Right 7
  it "Should throw an error when retrieving non-existent user" $ \testResults ->
    isLeft (badRetrieveResult testResults) `shouldBe` True
  it "Should throw an error when deleting non-existent user" $ \testResults ->
    isLeft (badDeleteResult testResults) `shouldBe` True

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
