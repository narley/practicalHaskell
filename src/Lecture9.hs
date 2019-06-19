{-# LANGUAGE OverloadedStrings #-}

module Lecture9 where

import Data.Aeson
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Schema

newtype UserWrapper = UserWrapper User
  deriving (Show, Eq)

instance ToJSON UserWrapper where
  toJSON = undefined

instance FromJSON UserWrapper where
  parseJSON = undefined


newtype ArticleWrapper = ArticleWrapper Article
  deriving (Show, Eq)

instance ToJSON ArticleWrapper where
  toJSON = undefined

instance FromJSON ArticleWrapper where
  parseJSON = undefined


newtype CommentWrapper = CommentWrapper Comment
  deriving (Show, Eq)

instance ToJSON CommentWrapper where
  toJSON = undefined

instance FromJSON CommentWrapper where
  parseJSON = undefined
