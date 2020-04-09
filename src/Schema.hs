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
    -- Exercise 10.d
    UniqueNameEmail name email
    deriving Show Read Eq

  Article sql=articles
    title Text
    body Text
    publishedAt UTCTime
    authorId UserId
    deriving Show Read Eq

  Comment sql=comments
    body Text
    submittedAt UTCTime
    userId UserId
    articleId ArticleId
    deriving Show Read Eq

-- Exercise 10.a
  ArticleReaction sql=articleReactions
    articleId ArticleId
    userId UserId Maybe
    type ReactionType
    metadata Metadata
    deriving Show Read Eq

|]

-- Exercise 9.b
deriveJSON (defaultOptions { fieldLabelModifier = dropXAndLowerFirst 4 }) ''User

deriveJSON (defaultOptions { fieldLabelModifier = dropXAndLowerFirst 7 }) ''Article

deriveJSON (defaultOptions { fieldLabelModifier = dropXAndLowerFirst 7 }) ''Comment

-- Exercise 10.a
deriveJSON (defaultOptions { fieldLabelModifier = dropXAndLowerFirst 15 }) ''ArticleReaction


-- Exercise 9.a
-- instance ToJSON User where
--   toJSON user = object
--     [ "name" .= userName user
--     , "email" .= userEmail user
--     , "age" .= userAge user
--     ]

-- instance FromJSON User where
--   parseJSON = withObject "User" $ \o -> do
--     name' <- o .: "name"
--     email' <- o .: "email"
--     age' <- o .: "age"
--     return $ User name' email' age'

-- instance ToJSON Article where
--   toJSON article = object
--     [ "title" .= articleTitle article
--     , "body" .= articleBody article
--     , "publishedAt" .= articlePublishedAt article
--     , "authorId" .= articleAuthorId article
--     ]

-- instance FromJSON Article where
--   parseJSON = withObject "Article" $ \o -> do
--     title' <- o .: "title"
--     body' <- o .: "body"
--     publishedAt' <- o .: "publishedAt"
--     authorId' <- o .: "authorId"
--     return $ Article title' body' publishedAt' authorId'

-- instance ToJSON Comment where
--   toJSON comment = object
--     [ "body" .= commentBody comment
--     , "submittedAt" .= commentSubmittedAt comment
--     , "userId" .= commentUserId comment
--     , "articleId" .= commentArticleId comment
--     ]

-- instance FromJSON Comment where
--   parseJSON = withObject "Comment" $ \o -> do
--     body' <- o .: "body"
--     submittedAt' <- o .: "submittedAt"
--     userId' <- o .: "userId"
--     articleId' <- o .: "articleId"
--     return $ Comment body' submittedAt' userId' articleId'
