module Lecture11 where

import Database.Persist.Migration
import Database.Persist.Migration.Postgres (runMigration)
import Database.Persist.Sql (Key, toSqlKey, get, insert)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Database (runAction, localConnString)
import Schema
import SchemaTypes

removeRatingOp :: Operation
removeRatingOp = undefined

removeRatingMigration :: Migration
removeRatingMigration =
  [ 0 ~> 1 := [ removeRatingOp ]
  ]

runRemoveMigration :: IO ()
runRemoveMigration = runAction localConnString $ do
  runMigration (MigrateSettings (const $ Just "remove_rating")) removeRatingMigration

doubleRatingOp :: Operation
doubleRatingOp = undefined

doubleRatingMigration :: Migration
doubleRatingMigration = undefined

runDoubleMigration :: IO ()
runDoubleMigration = runAction localConnString $ do
  runMigration (MigrateSettings (const $ Just "double_rating")) doubleRatingMigration

insertSurprised :: IO (Key ArticleReaction)
insertSurprised = runAction localConnString $ do
  insert (ArticleReaction Nothing (toSqlKey undefined) (undefined)
    (Metadata (posixSecondsToUTCTime 0) 0 0 0))

fetchSurprised :: IO (Maybe ArticleReaction)
fetchSurprised = runAction localConnString $ do
  get (toSqlKey undefined)

surprisedToLikeOp :: Operation
surprisedToLikeOp = undefined

surprisedToLikeMigration :: Migration
surprisedToLikeMigration = undefined

runSurprisedToLike :: IO ()
runSurprisedToLike = runAction localConnString $ do
  runMigration (MigrateSettings (const $ Just "surprise_to_like")) surprisedToLikeMigration
