{-# LANGUAGE OverloadedStrings #-}

module Lecture8 where

import Control.Monad.Logger (LoggingT)
import Data.Time (UTCTime(..), Day(..))
import Database.Persist (Entity(..), Key)
import Database.Persist.Postgresql (SqlPersistT)
import Database.Esqueleto

import Database
import Schema

lastYearsArticles :: SqlPersistT (LoggingT IO) [Entity Article]
lastYearsArticles = undefined

getYoungUsers :: SqlPersistT (LoggingT IO) [Entity User]
getYoungUsers = undefined

getSpecialPairs :: SqlPersistT (LoggingT IO) [(Entity User, Entity Article)]
getSpecialPairs = undefined

getCommentsFromUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsFromUser userId = undefined

getCommentsOnUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsOnUser userId = undefined
