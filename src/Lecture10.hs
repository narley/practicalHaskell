{-# LANGUAGE OverloadedStrings #-}

module Lecture10 where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist (insert, get, delete, selectList, Entity(..), SelectOpt(..))
import Database.Persist.Sql (toSqlKey)

import Database (runAction, localConnString)
import Schema
import SchemaTypes

md :: Metadata
md = Metadata
  { reactionTime = posixSecondsToUTCTime 1548343664
  , previousLikes = 5
  , previousLoves = 3
  , previousDislikes = 2
  }

aReaction :: ArticleReaction
aReaction = ArticleReaction
  { articleReactionUserId = Just (toSqlKey 14)
  , articleReactionArticleId = toSqlKey 90
  , articleReactionType = Love
  , articleReactionMetadata = md
  }

addReaction :: IO ()
addReaction = runAction localConnString $ do
  key <- insert aReaction
  liftIO $ print key

fetchNewReaction :: Int64 -> IO ()
fetchNewReaction key = runAction localConnString $ do
  result <- get (toSqlKey key :: Key ArticleReaction)
  liftIO $ print result

deleteNewReaction :: Int64 -> IO ()
deleteNewReaction key = runAction localConnString $
  delete (toSqlKey key :: Key ArticleReaction)

makeClone :: IO ()
makeClone = do
  [Entity _ user] <- runAction localConnString (selectList [] [LimitTo 1])
  newKey <- runAction localConnString $
    insert (User (userName user) (userEmail user) (userAge user))
  print newKey

deleteNewUser :: Int64 -> IO ()
deleteNewUser key = runAction localConnString $
  delete (toSqlKey key :: Key User)
