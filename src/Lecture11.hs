{-# LANGUAGE OverloadedStrings #-}

module Lecture11 where

import Data.Int (Int64)
import Data.Text hiding (map)
import Database.Persist.Migration
import Database.Persist.Migration.Postgres (runMigration)
import Database.Persist.Sql (Key, toSqlKey, get, insert, Single(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Database (runAction, localConnString)
import Schema
import SchemaTypes

-- Exercise 11.d
removeRatingOp :: Operation
removeRatingOp = DropColumn ("comments", "rating")

removeRatingMigration :: Migration
removeRatingMigration =
  [ 0 ~> 1 := [ removeRatingOp ]
  ]

runRemoveMigration :: IO ()
runRemoveMigration = runAction localConnString $ do
  runMigration (MigrateSettings (const $ Just "remove_rating")) removeRatingMigration

-- Exercise 11.e
doubleRatingOp :: Operation
doubleRatingOp = RawOperation "Double comment rating" $
  map migrateRating <$> rawSql "SELECT id, rating FROM comments" []
  where
    migrateRating :: (Single Int64, Single Int64) -> MigrateSql
    migrateRating (Single id', Single rating) =
      let newRating = rating * 2
      in
        MigrateSql "UPDATE comments SET rating = ? where id = ?"
        [ PersistInt64 newRating, PersistInt64 id']

-- Exercise 11.e
doubleRatingMigration :: Migration
doubleRatingMigration =
  [ 1 ~> 2 := [ doubleRatingOp ]
  ]

runDoubleMigration :: IO ()
runDoubleMigration = runAction localConnString $ do
  runMigration (MigrateSettings (const $ Just "double_rating")) doubleRatingMigration

-- insertSurprised :: IO (Key ArticleReaction)
-- insertSurprised = runAction localConnString $ do
--   insert (ArticleReaction (toSqlKey 87) Nothing (Surprised)
--     (Metadata (posixSecondsToUTCTime 0) 0 0 0))

fetchSurprised :: IO (Maybe ArticleReaction)
fetchSurprised = runAction localConnString $ do
  get (toSqlKey 4)

surprisedToLikeOp :: Operation
surprisedToLikeOp = RawOperation "Replace Surprised with Like" $
  map migrateArticleReaction <$> rawSql "SELECT id, type FROM \"articleReactions\" WHERE type='\"Surprised\"'" []
  where
    migrateArticleReaction :: (Single Int64, Single Text) -> MigrateSql
    migrateArticleReaction (Single id', _) =
      MigrateSql "UPDATE \"articleReactions\" SET type = ? where id = ?"
      [ PersistText "\"Love\"", PersistInt64 id' ]

surprisedToLikeMigration :: Migration
surprisedToLikeMigration =
  [ 2 ~> 3 := [ surprisedToLikeOp ]
  ]

runSurprisedToLike :: IO ()
runSurprisedToLike = runAction localConnString $ do
  runMigration (MigrateSettings (const $ Just "surprise_to_like")) surprisedToLikeMigration
