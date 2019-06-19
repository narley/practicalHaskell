{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int (Int64)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql (toSqlKey, insert)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Database
import Schema
import SchemaTypes

main :: IO ()
main = do
  putStrLn "If these tests compile, you're done with the first part of the exercises!"
  putStrLn "Finish up by following the rest of the instructions!"

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

insertReaction :: IO (Key ArticleReaction)
insertReaction = runAction localConnString (insert aReaction)
