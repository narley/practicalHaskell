{-# LANGUAGE OverloadedStrings #-}

module Lecture5 where

import Control.Monad.Logger (LoggingT)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (insert, delete, get, selectList, Key, (<.), SelectOpt(..))
import Database.Persist.Postgresql (SqlPersistT, Entity(..))

import Database (runAction, localConnString)
import Schema

myUser :: User
myUser = User "Lecture 5 User" "lec5@user.com" 23

insertAndPrintKey :: IO ()
insertAndPrintKey = undefined
-- where
--  myQuery :: SqlPersistT (LoggingT IO) (Key User)
--  myQuery = undefined
-- HINT Just define this query ^^, then use `runAction localConnString myQuery`,
-- which will be a normal IO action.

deleteNewUser :: IO ()
deleteNewUser = runAction localConnString (delete (undefined :: Key User))

fetch100 :: IO (Text, UTCTime)
fetch100 = do
  article <- runAction localConnString fetchQuery
  -- TODO Return the title and published date!
  return undefined
  where
    -- fetchQuery :: ???
    fetchQuery = undefined

-- TODO UNCOMMENT ME!
lastYearsArticles :: IO [Entity Article]
lastYearsArticles = runAction localConnString query
  where
    query = undefined

getYoungUsers :: IO [Entity User]
getYoungUsers = runAction localConnString query
  where
    query = undefined
