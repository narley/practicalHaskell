module Lecture7 where

import Control.Monad
import Control.Monad.Logger (LoggingT)
import Database.Persist (Entity(..), (<.), (>.), (==.), selectList, Filter(..), SelectOpt(..))
import Database.Persist.Postgresql (SqlPersistT)

import Database (runAction, localConnString)
import Schema

getCommentsFromUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsFromUser uid =
  selectList
    [ CommentUserId ==. uid]
    [ Asc CommentArticleId, Desc CommentSubmittedAt ]

getCommentsOnUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsOnUser userId = do
  articles <- selectList [ArticleAuthorId ==. userId] []
  selectList [commentsOnArticles articles] [Asc CommentSubmittedAt]
  where commentsOnArticles :: [Entity Article] -> Filter Comment
        commentsOnArticles = FilterOr . fmap ((CommentArticleId ==.) . entityKey)
