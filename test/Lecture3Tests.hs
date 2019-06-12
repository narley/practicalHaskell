{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Logger (LoggingT)
import Data.Time (getCurrentTime, UTCTime)
import Database.Persist (Key, Entity, insert, delete, selectList, (==.))
import Database.Persist.Postgresql (SqlPersistT)

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Database (localConnString, runAction)
import Schema

main :: IO ()
main = do
  time <- getCurrentTime
  defaultMain $ testGroup "Lecture 3 Tests"
    [ userTest, articleTest time ]

userTest :: TestTree
userTest = testCase "User Tests" $ do
  (key, entities) <- runAction localConnString $ do
    userKey <- insert user
    userEntities <- selectUserAction
    delete userKey
    return (userKey, userEntities)
  True @?= True

user :: User
user = User
  { userName = "James"
  , userEmail = "james@test.com"
  , userAge = 25
  }

selectUserAction :: SqlPersistT (LoggingT IO) [Entity User]
selectUserAction = selectList [UserName ==. "James", UserAge ==. 25] []

articleTest :: UTCTime -> TestTree
articleTest t = testCase "Article Tests" $ do
  (key, entities) <- runAction localConnString $ do
    articleKey <- insert (article t)
    articleEntities <- selectArticleAction
    delete articleKey
    return (articleKey, articleEntities)
  True @?= True

selectArticleAction :: SqlPersistT (LoggingT IO) [Entity Article]
selectArticleAction = selectList
  [ArticleTitle ==. "Introduction to Haskell", ArticleBody ==. "An excellent article"]
  []

article :: UTCTime -> Article
article t = Article
  { articleTitle = "Introduction to Haskell"
  , articleBody = "An excellent article"
  , articlePublishedAt = t
  }
