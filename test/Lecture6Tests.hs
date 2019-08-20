{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Either (isRight, fromRight)
import Data.Maybe (isJust, fromJust)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql (toSqlKey, Entity)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client (ClientEnv(..), parseBaseUrl, runClientM, client, ClientM, ServantError)
import Test.Hspec

import CrudServer (runServer, fullCRUD)
import Schema
import TestUtils (fromRight')

main :: IO ()
main = do
  tid <- forkIO runServer
  threadDelay 1000000
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8080"
  let clientEnv = ClientEnv manager baseUrl Nothing
  hspec $ beforeAll (fetchTestResults clientEnv) testSpec
  killThread tid

testSpec :: SpecWith TestResults
testSpec = describe "Calling the CRUD API gives the right results" $ do
  it "Should retrieve the right number of users" $ \testResults ->
    fetchAllUsersResult testResults `shouldBe` 101
  it "Should create the user properly" $ \testResults ->
    createUserResult testResults `shouldBe` True
  it "Should retrieve the user properly" $ \testResults -> do
    isRight (retrieveUserResult testResults) `shouldBe` True
    (isJust . fromRight' . retrieveUserResult $ testResults) `shouldBe` True
    let u = fromJust . fromRight' . retrieveUserResult $ testResults
    userName u `shouldBe` "Lecture 6 User"
    userEmail u `shouldBe` "lecture6@test.com"
    userAge u `shouldBe` 30
  it "Should update the user properly" $ \testResults -> do
    isRight (updateUserResult testResults) `shouldBe` True
    (isJust . fromRight' . updateUserResult $ testResults) `shouldBe` True
    let u = fromJust . fromRight' . updateUserResult $ testResults
    userName u `shouldBe` "Lecture 6 User (Updated)"
  it "Should delete the user properly" $ \testResults ->
    deleteUserResult testResults `shouldBe` True

  it "Should retrieve the right number of articles" $ \testResults ->
    fetchAllArticlesResult testResults `shouldBe` 100
  it "Should create the article properly" $ \testResults ->
    createArticleResult testResults `shouldBe` True
  it "Should retrieve the article properly" $ \testResults -> do
    isRight (retrieveArticleResult testResults) `shouldBe` True
    (isJust . fromRight' . retrieveArticleResult $ testResults) `shouldBe` True
    let a = fromJust . fromRight' . retrieveArticleResult $ testResults
    articleTitle a `shouldBe` "A Test of lecture 6"
    articleBody a `shouldBe` "Testing lecture 6"
  it "Should update the article properly" $ \testResults -> do
    isRight (updateArticleResult testResults) `shouldBe` True
    (isJust . fromRight' . updateArticleResult $ testResults) `shouldBe` True
    let a = fromJust . fromRight' . updateArticleResult $ testResults
    articleTitle a `shouldBe` "A Test of Lecture 6 (Updated)"
  it "Should delete the article properly" $ \testResults ->
    deleteArticleResult testResults `shouldBe` True

  it "Should retrieve the right number of comments" $ \testResults ->
    fetchAllCommentsResult testResults `shouldBe` 200
  it "Should create the comment properly" $ \testResults ->
    createCommentResult testResults `shouldBe` True
  it "Should retrieve the comment properly" $ \testResults -> do
    isRight (retrieveCommentResult testResults) `shouldBe` True
    (isJust . fromRight' . retrieveCommentResult $ testResults) `shouldBe` True
    let c = fromJust . fromRight' . retrieveCommentResult $ testResults
    commentBody c `shouldBe` "Great Article!"
  it "Should update the comment properly" $ \testResults -> do
    isRight (updateCommentResult testResults) `shouldBe` True
    (isJust . fromRight' . updateCommentResult $ testResults) `shouldBe` True
    let c = fromJust . fromRight' . updateCommentResult $ testResults
    commentBody c `shouldBe` "Decent Article!"
  it "Should delete the comment properly" $ \testResults ->
    deleteCommentResult testResults `shouldBe` True

data TestResults = TestResults
  { fetchAllUsersResult :: Int
  , createUserResult :: Bool
  , retrieveUserResult :: Either ServantError (Maybe User)
  , updateUserResult :: Either ServantError (Maybe User)
  , deleteUserResult :: Bool
  , fetchAllArticlesResult :: Int
  , createArticleResult :: Bool
  , retrieveArticleResult :: Either ServantError (Maybe Article)
  , updateArticleResult :: Either ServantError (Maybe Article)
  , deleteArticleResult :: Bool
  , fetchAllCommentsResult :: Int
  , createCommentResult :: Bool
  , retrieveCommentResult :: Either ServantError (Maybe Comment)
  , updateCommentResult :: Either ServantError (Maybe Comment)
  , deleteCommentResult :: Bool
  }

userToCreate :: User
userToCreate = User "Lecture 6 User" "lecture6@test.com" 30

updatedUser :: User
updatedUser = userToCreate { userName = "Lecture 6 User (Updated)" }

articleToCreate :: Int64 -> Article
articleToCreate uid = Article "A Test of lecture 6" "Testing lecture 6" (posixSecondsToUTCTime 0) (toSqlKey uid)

updatedArticle :: Int64 -> Article
updatedArticle uid = (articleToCreate uid) { articleTitle = "A Test of Lecture 6 (Updated)" }

commentToCreate :: Int64 -> Int64 -> Comment
commentToCreate uid aid = Comment (toSqlKey uid) (toSqlKey aid) "Great Article!" (posixSecondsToUTCTime 0)

updatedComment :: Int64 -> Int64 -> Comment
updatedComment uid aid = (commentToCreate uid aid) { commentBody = "Decent Article!" }

fetchTestResults :: ClientEnv -> IO TestResults
fetchTestResults clientEnv = do
  allUsers <- runClientM fetchAllUsersClient clientEnv
  let userCount = case allUsers of
        (Left _) -> -1
        (Right users) -> length users
  allArticles <- runClientM fetchAllArticlesClient clientEnv
  let articleCount = case allArticles of
        (Left _) -> -1
        (Right articles) -> length articles
  allComments <- runClientM fetchAllCommentsClient clientEnv
  let commentCount = case allComments of
        (Left _) -> -1
        (Right comments) -> length comments

  createUserResult <- runClientM (createUserClient userToCreate) clientEnv
  let (createdUserId, createdUserResult) = case createUserResult of
        (Left _) -> (1000000, False)
        (Right uid) -> (uid, True)
  retrievedUserResult <- runClientM (retrieveUserClient createdUserId) clientEnv
  updateUserResult <- runClientM (updateUserClient createdUserId updatedUser) clientEnv
  retrievedUserUpdatedResult <- runClientM (retrieveUserClient createdUserId) clientEnv

  createArticleResult <- runClientM (createArticleClient (articleToCreate createdUserId)) clientEnv
  let (createdArticleId, createdArticleResult) = case createArticleResult of
        (Left _) -> (1000000, False)
        (Right aid) -> (aid, True)
  retrievedArticleResult <- runClientM (retrieveArticleClient createdArticleId) clientEnv
  updateArticleResult <- runClientM (updateArticleClient createdArticleId (updatedArticle createdUserId)) clientEnv
  retrievedArticleUpdatedResult <- runClientM (retrieveArticleClient createdArticleId) clientEnv

  createCommentResult <- runClientM (createCommentClient (commentToCreate createdUserId createdArticleId)) clientEnv
  let (createdCommentId, createdCommentResult) = case createCommentResult of
        (Left _) -> (1000000, False)
        (Right cid) -> (cid, True)
  retrievedCommentResult <- runClientM (retrieveCommentClient createdCommentId) clientEnv
  updateCommentResult <- runClientM (updateCommentClient createdCommentId (updatedComment createdUserId createdArticleId)) clientEnv
  retrievedCommentUpdatedResult <- runClientM (retrieveCommentClient createdCommentId) clientEnv

  deleteCommentResult <- runClientM (deleteCommentClient createdCommentId) clientEnv
  retrievedCommentDeletedResult <- runClientM (retrieveCommentClient createdCommentId) clientEnv
  let deletedCommentResult = case retrievedCommentDeletedResult of
        (Right Nothing) -> True
        _ -> False
  deleteArticleResult <- runClientM (deleteArticleClient createdArticleId) clientEnv
  retrievedArticleDeletedResult <- runClientM (retrieveArticleClient createdArticleId) clientEnv
  let deletedArticleResult = case retrievedArticleDeletedResult of
        (Right Nothing) -> True
        _ -> False
  deleteUserResult <- runClientM (deleteUserClient createdUserId) clientEnv
  retrievedUserDeletedResult <- runClientM (retrieveUserClient createdUserId) clientEnv
  let deletedUserResult = case retrievedUserDeletedResult of
        (Right Nothing) -> True
        _ -> False

  return $ TestResults userCount createdUserResult retrievedUserResult retrievedUserUpdatedResult deletedUserResult
    articleCount createdArticleResult retrievedArticleResult retrievedArticleUpdatedResult deletedArticleResult
    commentCount createdCommentResult retrievedCommentResult retrievedCommentUpdatedResult deletedCommentResult

fetchAllUsersClient :: ClientM [Entity User]
retrieveUserClient :: Int64 -> ClientM (Maybe User)
createUserClient :: User -> ClientM Int64
deleteUserClient :: Int64 -> ClientM ()
updateUserClient :: Int64 -> User -> ClientM ()

fetchAllArticlesClient :: ClientM [Entity Article]
retrieveArticleClient :: Int64 -> ClientM (Maybe Article)
createArticleClient :: Article -> ClientM Int64
deleteArticleClient :: Int64 -> ClientM ()
updateArticleClient :: Int64 -> Article -> ClientM ()

fetchAllCommentsClient :: ClientM [Entity Comment]
retrieveCommentClient :: Int64 -> ClientM (Maybe Comment)
createCommentClient :: Comment -> ClientM Int64
deleteCommentClient :: Int64 -> ClientM ()
updateCommentClient :: Int64 -> Comment -> ClientM ()

(fetchAllUsersClient :<|>
 retrieveUserClient :<|>
 createUserClient :<|>
 deleteUserClient :<|>
 updateUserClient) :<|>
 (fetchAllArticlesClient :<|>
 retrieveArticleClient :<|>
 createArticleClient :<|>
 deleteArticleClient :<|>
 updateArticleClient) :<|>
 (fetchAllCommentsClient :<|>
 retrieveCommentClient :<|>
 createCommentClient :<|>
 deleteCommentClient :<|>
 updateCommentClient) = client fullCRUD
