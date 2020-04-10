{-# LANGUAGE TemplateHaskell #-}
module SchemaTypes where

import Data.Aeson.TH
import Data.Time (UTCTime)
import Database.Persist.TH
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), object, (.:), withObject, withArray)
import Data.Text

data ReactionType
  = Like
  | Love
  | Dislike
  -- Exercise 11.f
  -- | Surprised
  deriving (Show, Read, Eq)

-- Exercise 10.a
-- instance ToJSON ReactionType where
--   toJSON Like = String $ pack "like"
--   toJSON Love = String $ pack "love"
--   toJSON Dislike = String $ pack "dislike"

-- instance FromJSON ReactionType where
--   parseJSON = withObject "ReactionType" $ \o -> do
--     type' <- o .: pack "type"
--     case type' of
--       "like" -> return Like
--       "love" -> return Love
--       "dislike" -> return Dislike
--       _ -> undefined

-- Exercise 10.b
data Metadata = Metadata
  { reactionTime :: UTCTime
  , previousLikes :: Int
  , previousLoves :: Int
  , previousDislikes :: Int
  } deriving (Show, Read, Eq)

-- Exercise 10.b
-- instance ToJSON Metadata where
--   toJSON metadata = object
--     [ pack "reactionTime" .= reactionTime metadata
--     , pack "previousLikes" .= previousLikes metadata
--     , pack "previousLoves" .= previousLoves metadata
--     , pack "previousDislikes" .= previousDislikes metadata
--     ]

-- instance FromJSON Metadata where
--   parseJSON = withObject "Metadata" $ \o -> do
--     reactionTime' <- o .: pack "reactionTime"
--     previousLikes' <- o .: pack "previousLikes"
--     previousLoves' <- o .: pack "previousLoves"
--     previousDislikes' <- o .: pack "previousDislikes"
--     return $ Metadata reactionTime' previousLikes' previousLoves' previousDislikes'

-- Exercise 10.c
-- derivePersistField "Metadata"
-- derivePersistField "ReactionType"

-- Exercise 10.d
deriveJSON defaultOptions ''ReactionType
deriveJSON defaultOptions ''Metadata

-- Exercice 10.d
derivePersistFieldJSON "Metadata"
derivePersistFieldJSON "ReactionType"

{-
Utils> fetchNewReaction 1
Just (ArticleReaction {articleReactionArticleId = ArticleKey {unArticleKey = SqlBackendKey {unSqlBackendKey = 90}}, articleReactionUserId = Just (UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 14}}), articleReactionType = Love, articleReactionMetadata = Metadata {reactionTime = 2019-01-24 15:27:44 UTC, previousLikes = 5, previousLoves = 3, previousDislikes = 2}})

Utils> fetchNewReaction 3
Just (ArticleReaction {articleReactionArticleId = ArticleKey {unArticleKey = SqlBackendKey {unSqlBackendKey = 90}}, articleReactionUserId = Just (UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 14}}), articleReactionType = Love, articleReactionMetadata = Metadata {reactionTime = 2019-01-24 15:27:44 UTC, previousLikes = 5, previousLoves = 3, previousDislikes = 2}})
-}
