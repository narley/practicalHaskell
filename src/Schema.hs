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
|]
