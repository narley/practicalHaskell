{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Persist.Sql (toSqlKey)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Lecture7

import DatabaseTestUtils

main :: IO()
main = defaultMain $ testGroup "Lecture 7 Tests"
  [commentsFromUserTests, commentsOnUserTests]

commentsFromUserTests :: TestTree
commentsFromUserTests = testCase "Comments For User Tests" $ do
  comments1 <- rq (getCommentsFromUser (toSqlKey 2))
  comments2 <- rq (getCommentsFromUser (toSqlKey 5))
  comments3 <- rq (getCommentsFromUser (toSqlKey 37))
  comments4 <- rq (getCommentsFromUser (toSqlKey 63))
  comments5 <- rq (getCommentsFromUser (toSqlKey 98))
  comments1 @?= trueComments1
  comments2 @?= trueComments2
  comments3 @?= trueComments3
  comments4 @?= trueComments4
  comments5 @?= trueComments5

commentsOnUserTests :: TestTree
commentsOnUserTests = testCase "Comments On User Tests" $ do
  comments1 <- rq (getCommentsOnUser (toSqlKey 4))
  comments2 <- rq (getCommentsOnUser (toSqlKey 8))
  comments3 <- rq (getCommentsOnUser (toSqlKey 16))
  comments4 <- rq (getCommentsOnUser (toSqlKey 32))
  comments5 <- rq (getCommentsOnUser (toSqlKey 66))
  comments1 @?= trueComments6
  comments2 @?= trueComments7
  comments3 @?= trueComments8
  comments4 @?= trueComments9
  comments5 @?= trueComments10
