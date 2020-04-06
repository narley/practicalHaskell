{-# LANGUAGE OverloadedStrings #-}

module Lecture4 where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist (Entity(..), selectList)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import System.IO (Handle, hPrint)

import Database (runAction, localConnString)
import Schema

myUser :: User
myUser = User
  { userName = "Chris"
  , userEmail = "chris@mail.com"
  , userAge = 30
  }

myArticle :: Article
myArticle = Article
  { articleTitle = "Intro to Haskell"
  , articleBody = "Intro to Haskell body"
  , articlePublishedAt = posixSecondsToUTCTime 1584309368012
  }

myUserEntity :: Entity User
myUserEntity = Entity (toSqlKey 1) myUser

myArticleEntity :: Entity Article
myArticleEntity = Entity (toSqlKey 1) myArticle

fetchAllUsers :: IO [Entity User]
fetchAllUsers = runAction localConnString $ selectList [] []

printAllKeys :: Handle -> IO ()
printAllKeys handle = do
  users <- fetchAllUsers
  let keys = map (fromSqlKey . entityKey) users
  hPrint handle keys
