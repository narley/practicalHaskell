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
import           Data.Int (Int64)
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

  LoginToken sql=login_tokens
    userId UserId
    cookie Text
    UniqueUserid userId

|]

data LoginInfo = LoginInfo
  { loginInfoUsername :: Text
  , loginInfoPassword :: Text
  }

instance ToJSON LoginInfo where
  toJSON (LoginInfo username password)= object
    [ "loginInfoUsername" .= username
    , "loginInfoPassword" .= password
    ]

instance FromJSON LoginInfo where
  parseJSON = withObject "Login Info" $ \o -> do
    username <- o .: "loginInfoUsername"
    password <- o .: "loginInfoPassword"
    return $ LoginInfo username password

data LoginResponse = LoginResponse
  { loginResponseUserId :: Int64
  , loginResponseCookie :: Text
  }

instance ToJSON LoginResponse where
  toJSON (LoginResponse userId cookie)= object
    [ "loginResponseUserId" .= userId
    , "loginResponseCookie" .= cookie
    ]

instance FromJSON LoginResponse where
  parseJSON = withObject "Login Response" $ \o -> do
    userId <- o .: "loginResponseUserId"
    cookie <- o .: "loginResponseCookie"
    return $ LoginResponse userId cookie

userPairs :: User -> [Pair]
userPairs user =
  [ "userName" .= userName user
  , "userEmail" .= userEmail user
  , "userAge" .= userAge user
  ]

parseUser :: Object -> Parser User
parseUser o = do
  name_ <- o .: "userName"
  email_ <- o .: "userEmail"
  age_ <- o .: "userAge"
  return $ User name_ email_ age_

instance ToJSON User where
  toJSON = object . userPairs

instance FromJSON User where
  parseJSON = withObject "User" parseUser

instance ToJSON (Entity User) where
  toJSON (Entity uid user) = object $ [("key" .= fromSqlKey uid), ("value" .= user)]

instance FromJSON (Entity User) where
  parseJSON = withObject "User" $ \o -> do
    user <- o .: "value"
    uid <- o .: "id"
    return $ Entity (toSqlKey uid) user

articlePairs :: Article -> [Pair]
articlePairs article =
  [ "articleTitle" .= articleTitle article
  , "articleBody" .= articleBody article
  , "articlePublishedAt" .= articlePublishedAt article
  , "articleAuthorId" .= fromSqlKey (articleAuthorId article)
  ]

parseArticle :: Object -> Parser Article
parseArticle o = do
    title_ <- o .: "articleTitle"
    body_ <- o .: "articleBody"
    publishedAt_ <- o .: "articlePublishedAt"
    authorId_ <- o .: "articleAuthorId"
    return $ Article title_ body_ publishedAt_ (toSqlKey authorId_)

instance ToJSON Article where
  toJSON = object . articlePairs

instance FromJSON Article where
  parseJSON = withObject "Article" parseArticle

instance ToJSON (Entity Article) where
  toJSON (Entity aid article) = object $ [("key" .= fromSqlKey aid), ("value" .= article)]

instance FromJSON (Entity Article) where
  parseJSON = withObject "Article" $ \o -> do
    article <- o .: "value"
    aid <- o .: "id"
    return $ Entity (toSqlKey aid) article

commentPairs :: Comment -> [Pair]
commentPairs comment =
  [ "commentArticleId" .= fromSqlKey (commentArticleId comment)
  , "commentBody" .= commentBody comment
  , "commentSubmittedAt" .= commentSubmittedAt comment
  , "commentUserId" .= fromSqlKey (commentUserId comment)
  ]

parseComment :: Object -> Parser Comment
parseComment o = do
  userId_ <- o .: "commentUserId"
  body_ <- o .: "commentBody"
  submittedAt_ <- o .: "commentSubmittedAt"
  articleId_ <- o .: "commentArticleId"
  return $ Comment (toSqlKey userId_) (toSqlKey articleId_) body_ submittedAt_

instance ToJSON Comment where
  toJSON = object . commentPairs

instance FromJSON Comment where
  parseJSON = withObject "Comment" parseComment

instance ToJSON (Entity Comment) where
  toJSON (Entity cid comment) = object $ [("key" .= fromSqlKey cid), ("value" .= comment)]

instance FromJSON (Entity Comment) where
  parseJSON = withObject "Comment" $ \o -> do
    comment <- o .: "value"
    cid <- o .: "id"
    return $ Entity (toSqlKey cid) comment
