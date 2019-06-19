{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql (toSqlKey)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Schema
import Lecture9

main :: IO ()
main = defaultMain $ testGroup "Lecture 9 Tests"
  [ userJSONBasicTests
  , articleJSONBasicTests
  , commentJSONBasicTests
  , userJSONWrapperTests
  , articleJSONWrapperTests
  , commentJSONWrapperTests
  ]

userJSONBasicTests :: TestTree
userJSONBasicTests = testCase "User JSON Basic Tests" $ do
  toJSON user1 @?= userJSON1
  toJSON user2 @?= userJSON2
  fromJSON userJSON1 @?= Success user1
  fromJSON userJSON2 @?= Success user2

user1 :: User
user1 = User "James" "james@test.com" 26

user2 :: User
user2 = User "Kristin" "kristin@gmail.com" 28

userJSON1 :: Value
userJSON1 = object
  [ "name" .= ("James" :: Text)
  , "email" .= ("james@test.com" :: Text)
  , "age" .= (26 :: Int)
  ]

userJSON2 :: Value
userJSON2 = object
  [ "name" .= ("Kristin" :: Text)
  , "email" .= ("kristin@gmail.com" :: Text)
  , "age" .= (28 :: Int)
  ]

articleJSONBasicTests :: TestTree
articleJSONBasicTests = testCase "Article JSON Basic Tests" $ do
  toJSON article1 @?= articleJSON1
  toJSON article2 @?= articleJSON2
  fromJSON articleJSON1 @?= Success article1
  fromJSON articleJSON2 @?= Success article2

article1 :: Article
article1 = Article
  "A Great Article"
  "This article talks about things."
  (posixSecondsToUTCTime 1553117945)
  (toSqlKey 3)

article2 :: Article
article2 = Article
  "A Quick Review"
  "We need to understand what's already happened."
  (posixSecondsToUTCTime 1553118945)
  (toSqlKey 8)

articleJSON1 :: Value
articleJSON1 = object
  [ "title" .= ("A Great Article" :: Text)
  , "body" .= ("This article talks about things." :: Text)
  , "publishedAt" .= posixSecondsToUTCTime 1553117945
  , "authorId" .= (3 :: Int64)
  ]

articleJSON2 :: Value
articleJSON2 = object
  [ "title" .= ("A Quick Review" :: Text)
  , "body" .= ("We need to understand what's already happened." :: Text)
  , "publishedAt" .= posixSecondsToUTCTime 1553118945
  , "authorId" .= (8 :: Int64)
  ]

commentJSONBasicTests :: TestTree
commentJSONBasicTests = testCase "Comment JSON Basic Tests" $ do
  toJSON comment1 @?= commentJSON1
  toJSON comment2 @?= commentJSON2
  fromJSON commentJSON1 @?= Success comment1
  fromJSON commentJSON2 @?= Success comment2

comment1 :: Comment
comment1 = Comment
  { commentBody = "This was a good read."
  , commentUserId = (toSqlKey 3)
  , commentArticleId = (toSqlKey 2)
  , commentSubmittedAt = (posixSecondsToUTCTime 1553119945)
  }

comment2 :: Comment
comment2 = Comment
  { commentBody = "This article was fine."
  , commentUserId = (toSqlKey 8)
  , commentArticleId = (toSqlKey 4)
  , commentSubmittedAt = (posixSecondsToUTCTime 1553120945)
  }

commentJSON1 :: Value
commentJSON1 = object
  [ "body" .= ("This was a good read." :: Text)
  , "userId" .= (3 :: Int64)
  , "articleId" .= (2 :: Int64)
  , "submittedAt" .= posixSecondsToUTCTime 1553119945
  ]

commentJSON2 :: Value
commentJSON2 = object
  [ "body" .= ("This article was fine." :: Text)
  , "userId" .= (8 :: Int64)
  , "articleId" .= (4 :: Int64)
  , "submittedAt" .= posixSecondsToUTCTime 1553120945
  ]

userJSONWrapperTests :: TestTree
userJSONWrapperTests = testCase "User JSON Wrapper Tests" $ do
  toJSON (UserWrapper user1) @?= userWrapperJSON1
  toJSON (UserWrapper user2) @?= userWrapperJSON2
  fromJSON userWrapperJSON1 @?= Success (UserWrapper user1)
  fromJSON userWrapperJSON2 @?= Success (UserWrapper user2)

userWrapperJSON1 :: Value
userWrapperJSON1 = object
  [ "userName" .= ("James" :: Text)
  , "userEmail" .= ("james@test.com" :: Text)
  , "userAge" .= (26 :: Int)
  ]

userWrapperJSON2 :: Value
userWrapperJSON2 = object
  [ "userName" .= ("Kristin" :: Text)
  , "userEmail" .= ("kristin@gmail.com" :: Text)
  , "userAge" .= (28 :: Int)
  ]

articleJSONWrapperTests :: TestTree
articleJSONWrapperTests = testCase "Article JSON Wrapper Tests" $ do
  toJSON (ArticleWrapper article1) @?= articleWrapperJSON1
  toJSON (ArticleWrapper article2) @?= articleWrapperJSON2
  fromJSON articleWrapperJSON1 @?= Success (ArticleWrapper article1)
  fromJSON articleWrapperJSON2 @?= Success (ArticleWrapper article2)

articleWrapperJSON1 :: Value
articleWrapperJSON1 = object
  [ "articleTitle" .= ("A Great Article" :: Text)
  , "articleBody" .= ("This article talks about things." :: Text)
  , "articlePublishedAt" .= posixSecondsToUTCTime 1553117945
  , "articleAuthorId" .= (3 :: Int64)
  ]

articleWrapperJSON2 :: Value
articleWrapperJSON2 = object
  [ "articleTitle" .= ("A Quick Review" :: Text)
  , "articleBody" .= ("We need to understand what's already happened." :: Text)
  , "articlePublishedAt" .= posixSecondsToUTCTime 1553118945
  , "articleAuthorId" .= (8 :: Int64)
  ]

commentJSONWrapperTests :: TestTree
commentJSONWrapperTests = testCase "Comment JSON Wrapper Tests" $ do
  toJSON (CommentWrapper comment1) @?= commentWrapperJSON1
  toJSON (CommentWrapper comment2) @?= commentWrapperJSON2
  fromJSON commentWrapperJSON1 @?= Success (CommentWrapper comment1)
  fromJSON commentWrapperJSON2 @?= Success (CommentWrapper comment2)

commentWrapperJSON1 :: Value
commentWrapperJSON1 = object
  [ "commentBody" .= ("This was a good read." :: Text)
  , "commentUserId" .= (3 :: Int64)
  , "commentArticleId" .= (2 :: Int64)
  , "commentSubmittedAt" .= posixSecondsToUTCTime 1553119945
  ]

commentWrapperJSON2 :: Value
commentWrapperJSON2 = object
  [ "commentBody" .= ("This article was fine." :: Text)
  , "commentUserId" .= (8 :: Int64)
  , "commentArticleId" .= (4 :: Int64)
  , "commentSubmittedAt" .= posixSecondsToUTCTime 1553120945
  ]
