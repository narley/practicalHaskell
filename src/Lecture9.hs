{-# LANGUAGE OverloadedStrings #-}

module Lecture9 where

import Data.Aeson
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Schema

newtype UserWrapper = UserWrapper User
  deriving (Show, Eq)


-- Exercise 9.c
instance ToJSON UserWrapper where
  toJSON (UserWrapper user) = object
    [ "userName" .= userName user
    , "userEmail" .= userEmail user
    , "userAge" .= userAge user
    ]

instance FromJSON UserWrapper where
  parseJSON = withObject "UserWrapper" $ \o -> do
    name <- o .: "userName"
    email <- o .: "userEmail"
    age <- o .: "userAge"
    return $ UserWrapper (User name email age)


newtype ArticleWrapper = ArticleWrapper Article
  deriving (Show, Eq)

instance ToJSON ArticleWrapper where
  toJSON (ArticleWrapper article) = object
    [ "articleTitle" .= articleTitle article
    , "articleBody" .= articleBody article
    , "articlePublishedAt" .= articlePublishedAt article
    , "articleAuthorId" .= articleAuthorId article
    ]

instance FromJSON ArticleWrapper where
  parseJSON = withObject "ArticleWrapper" $ \o -> do
    title <- o .: "articleTitle"
    body <- o .: "articleBody"
    publishedAt <- o .: "articlePublishedAt"
    authorId <- o .: "articleAuthorId"
    return $ ArticleWrapper (Article title body publishedAt authorId)


newtype CommentWrapper = CommentWrapper Comment
  deriving (Show, Eq)

instance ToJSON CommentWrapper where
  toJSON (CommentWrapper comment) = object
    [ "commentBody" .= commentBody comment
    , "commentSubmittedAt" .= commentSubmittedAt comment
    , "commentUserId" .= commentUserId comment
    , "commentArticleId" .= commentArticleId comment
    ]

instance FromJSON CommentWrapper where
  parseJSON = withObject "CommentWrapper" $ \o -> do
    body <- o .: "commentBody"
    submittedAt <- o .: "commentSubmittedAt"
    userId <- o .: "commentUserId"
    articleId <- o .: "commentArticleId"
    return $ CommentWrapper (Comment body submittedAt userId articleId)
