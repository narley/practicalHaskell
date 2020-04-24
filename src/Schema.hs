{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Schema where

import qualified Database.Persist.TH as PTH
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), object, (.:), withObject, withArray, Object)
import           Data.Aeson.Types (Parser, Pair)
import           Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import           Data.ByteString (ByteString)
import           Database.Persist.Sql (Key, Entity(..), fromSqlKey, toSqlKey)
import           Data.Text (Text)
import           Data.Time (UTCTime)

import           Utils (dropXAndLowerFirst)

import SchemaTypes

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|

  User sql=users
    name Text
    email Text
    age Int
    deriving Show Read Eq
    UniqueEmail email

  Article sql=articles
    title Text
    body Text
    publishedAt UTCTime
    authorId UserId
    deriving Show Read Eq

  Comment sql=comments
    userId UserId
    articleId ArticleId
    body Text
    submittedAt UTCTime
    deriving Show Read Eq

  ArticleReaction sql=article_reactions
    articleId ArticleId
    userId UserId Maybe
    type ReactionType
    metadata Metadata
    deriving Show Read Eq

  AuthData sql=auth_data
    userId UserId
    hashString ByteString
    userType Text
    UniqueUserId userId
    deriving Show Read Eq

  LoginToken sql=login_tokens
    userId UserId
    cookie Text
    UniqueUserid userId

|]

data LoginInfo = LoginInfo Text Text

instance ToJSON LoginInfo where
  toJSON (LoginInfo username password)= object
    [ "username" .= username
    , "password" .= password
    ]

instance FromJSON LoginInfo where
  parseJSON = withObject "Login Info" $ \o -> do
    username <- o .: "username"
    password <- o .: "password"
    return $ LoginInfo username password

userPairs :: User -> [Pair]
userPairs user =
  [ "name" .= userName user
  , "email" .= userEmail user
  , "age" .= userAge user
  ]

parseUser :: Object -> Parser User
parseUser o = do
  name_ <- o .: "name"
  email_ <- o .: "email"
  age_ <- o .: "age"
  return $ User name_ email_ age_

instance ToJSON User where
  toJSON = object . userPairs

instance FromJSON User where
  parseJSON = withObject "User" parseUser

instance ToJSON (Entity User) where
  toJSON (Entity uid user) = object $ ("id" .= fromSqlKey uid) : userPairs user

instance FromJSON (Entity User) where
  parseJSON = withObject "User" $ \o -> do
    user <- parseUser o
    uid <- o .: "id"
    return $ Entity (toSqlKey uid) user

articlePairs :: Article -> [Pair]
articlePairs article =
  [ "title" .= articleTitle article
  , "body" .= articleBody article
  , "publishedAt" .= articlePublishedAt article
  , "authorId" .= fromSqlKey (articleAuthorId article)
  ]

parseArticle :: Object -> Parser Article
parseArticle o = do
    title_ <- o .: "title"
    body_ <- o .: "body"
    publishedAt_ <- o .: "publishedAt"
    authorId_ <- o .: "authorId"
    return $ Article title_ body_ publishedAt_ (toSqlKey authorId_)

instance ToJSON Article where
  toJSON = object . articlePairs

instance FromJSON Article where
  parseJSON = withObject "Article" parseArticle

instance ToJSON (Entity Article) where
  toJSON (Entity aid article) = object $ ("id" .= fromSqlKey aid) : articlePairs article

instance FromJSON (Entity Article) where
  parseJSON = withObject "Article" $ \o -> do
    article <- parseArticle o
    aid <- o .: "id"
    return $ Entity (toSqlKey aid) article

commentPairs :: Comment -> [Pair]
commentPairs comment =
  [ "articleId" .= fromSqlKey (commentArticleId comment)
  , "body" .= commentBody comment
  , "submittedAt" .= commentSubmittedAt comment
  , "userId" .= fromSqlKey (commentUserId comment)
  ]

parseComment :: Object -> Parser Comment
parseComment o = do
  userId_ <- o .: "userId"
  body_ <- o .: "body"
  submittedAt_ <- o .: "submittedAt"
  articleId_ <- o .: "articleId"
  return $ Comment (toSqlKey userId_) (toSqlKey articleId_) body_ submittedAt_

instance ToJSON Comment where
  toJSON = object . commentPairs

instance FromJSON Comment where
  parseJSON = withObject "Comment" parseComment

instance ToJSON (Entity Comment) where
  toJSON (Entity cid comment) = object $ ("id" .= fromSqlKey cid) : commentPairs comment

instance FromJSON (Entity Comment) where
  parseJSON = withObject "Comment" $ \o -> do
    comment <- parseComment o
    cid <- o .: "id"
    return $ Entity (toSqlKey cid) comment
