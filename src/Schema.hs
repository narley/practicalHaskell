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
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), object, (.:), withObject, withArray)
import           Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
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
    UniqueUserNE name email

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

|]

instance ToJSON User where
  toJSON user = object
    [ "name" .= userName user
    , "email" .= userEmail user
    , "age" .= userAge user
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    name_ <- o .: "name"
    email_ <- o .: "email"
    age_ <- o .: "age"
    return $ User name_ email_ age_

instance ToJSON Article where
  toJSON article = object
    [ "title" .= articleTitle article
    , "body" .= articleBody article
    , "publishedAt" .= articlePublishedAt article
    , "authorId" .= (fromSqlKey (articleAuthorId article))
    ]

instance FromJSON Article where
  parseJSON = withObject "Article" $ \o -> do
    title_ <- o .: "title"
    body_ <- o .: "body"
    publishedAt_ <- o .: "publishedAt"
    authorId_ <- o .: "authorId"
    return $ Article title_ body_ publishedAt_ (toSqlKey authorId_)

instance ToJSON Comment where
  toJSON comment = object
    [ "articleId" .= (fromSqlKey (commentArticleId comment))
    , "body" .= commentBody comment
    , "submittedAt" .= commentSubmittedAt comment
    , "userId" .= (fromSqlKey (commentUserId comment))
    ]

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    userId_ <- o .: "userId"
    body_ <- o .: "body"
    submittedAt_ <- o .: "submittedAt"
    articleId_ <- o .: "articleId"
    return $ Comment (toSqlKey userId_) (toSqlKey articleId_) body_ submittedAt_
