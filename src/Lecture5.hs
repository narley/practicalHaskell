{-# LANGUAGE OverloadedStrings #-}

module Lecture5 where

import Control.Monad.Logger (LoggingT)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime(..), Day(..))
import Database.Persist (insert, delete, get, selectList, Key, (<.), SelectOpt(..))
import Database.Persist.Postgresql (SqlPersistT, Entity(..), toSqlKey, (>.), (<.), (==.))
import Database.Persist.Sql (fromSqlKey)

import Database (runAction, localConnString)
import Schema

myUser :: User
myUser = User "Lecture 5 User" "lec5@user.com" 23

insertAndPrintKey :: IO ()
insertAndPrintKey = do
  userKey <- runAction localConnString myQuery
  print $ fromSqlKey userKey
  where
  myQuery :: SqlPersistT (LoggingT IO) (Key User)
  myQuery = insert myUser
  -- HINT Just define this query ^^, then use `runAction localConnString myQuery`,
  -- which will be a normal IO action.

deleteNewUser :: IO ()
deleteNewUser = runAction localConnString (delete (toSqlKey 104 :: Key User))

fetch100 :: IO (Text, UTCTime)
fetch100 = do
  article <- runAction localConnString fetchQuery
  -- TODO Return the title and published date!
  let articleFromMaybe = fromJust article
  return (articleTitle articleFromMaybe, articlePublishedAt articleFromMaybe)
  where
    fetchQuery :: SqlPersistT (LoggingT IO) (Maybe Article)
    fetchQuery = get (toSqlKey 100 :: Key Article)

lastYearsArticles :: IO [Entity Article]
lastYearsArticles = runAction localConnString query
  where
    query :: SqlPersistT (LoggingT IO) [Entity Article]
    query = selectList
      [ ArticlePublishedAt >. UTCTime (ModifiedJulianDay 58119) 0
      , ArticlePublishedAt <. UTCTime (ModifiedJulianDay 58483) 0
      ]
      [ Desc ArticleTitle ]

getYoungUsers :: IO [Entity User]
getYoungUsers = runAction localConnString query
  where
    query :: SqlPersistT (LoggingT IO) [Entity User]
    query = do
      xs <- selectList
        [ UserAge <. 23 ]
        [ Asc UserName, LimitTo 10 ]
      return $ reverse xs
