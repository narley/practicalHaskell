{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Persist.Sql (fromSqlKey, entityKey)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Lecture4
import Schema

import TestUtils (testIOPrint)

main :: IO ()
main = defaultMain $ testGroup "Lecture 4 Tests"
  [myUserTests, myArticleTests]

myUserTests :: TestTree
myUserTests = testCase "My User Tests" $ do
  userName myUser @?= "Chris"
  (fromSqlKey . entityKey) myUserEntity @?= 1

myArticleTests :: TestTree
myArticleTests = testCase "My Article Tests" $ do
  articleTitle myArticle @?= "Intro to Haskell"
  (fromSqlKey . entityKey) myArticleEntity @?= 1

printAllKeysTests :: TestTree
printAllKeysTests = testCase "Print All Keys Tests" $
  testIOPrint printAllKeys (show <$> [1..100])
