{-# LANGUAGE OverloadedStrings #-}

module Lecture8 where

import Control.Monad.Logger (LoggingT)
import Data.Time (UTCTime(..), Day(..))
import Database.Persist (Entity(..), Key)
import Database.Persist.Postgresql (SqlPersistT)
import Database.Esqueleto

import qualified Data.Text as T
import Data.List

import Database
import Schema

lastYearsArticles :: SqlPersistT (LoggingT IO) [Entity Article]
lastYearsArticles = select . from $ \articles -> do
  where_ (articles ^. ArticlePublishedAt >. val (UTCTime (ModifiedJulianDay 58119) 0))
  where_ (articles ^. ArticlePublishedAt <. val (UTCTime (ModifiedJulianDay 58483) 0))
  orderBy [asc (articles ^. ArticleTitle)]
  return articles

getYoungUsers :: SqlPersistT (LoggingT IO) [Entity User]
getYoungUsers = select . from $ \users -> do
  where_ (users ^. UserAge <. val 23)
  orderBy [desc (users ^. UserName)]
  limit 10
  return users

getSpecialPairs :: SqlPersistT (LoggingT IO) [(Entity User, Entity Article)]
getSpecialPairs = select . from $ \(users `InnerJoin` articles) -> do
  on (users ^. UserId ==. articles ^. ArticleAuthorId)
  where_ ((users ^. UserName `like` val "E" ++. (%))
      &&. (articles ^. ArticleTitle `like` val "E" ++. (%))
      ||. (users ^. UserName `like` val "T" ++. (%))
      &&. (articles ^. ArticleTitle `like` val "T" ++. (%)))
  return (users, articles)

getCommentsFromUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsFromUser userId = select . from $ \comments -> do
  where_ (comments ^. CommentUserId ==. val userId)
  orderBy [asc (comments ^. CommentArticleId), desc (comments ^. CommentSubmittedAt)]
  return comments

getCommentsOnUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsOnUser userId = select . from $ \(comments `InnerJoin` articles ) -> do
  on $ articles ^. ArticleId ==. comments ^. CommentArticleId
  where_ (articles ^. ArticleAuthorId ==. val userId)
  orderBy [asc (comments ^. CommentSubmittedAt)]
  return comments
