module Lecture7 where

import Control.Monad
import Control.Monad.Logger (LoggingT)
import Data.List (sortBy)
import Database.Persist (Entity(..), (<.), (>.), (==.), selectList, Filter(..), SelectOpt(..))
import Database.Persist.Postgresql (SqlPersistT)

import Database (runAction, localConnString)
import Schema

getCommentsFromUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsFromUser userId = undefined

getCommentsOnUser :: Key User -> SqlPersistT (LoggingT IO) [Entity Comment]
getCommentsOnUser userId = undefined
