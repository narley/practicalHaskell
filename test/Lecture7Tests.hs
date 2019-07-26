{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Either (isRight, isLeft)
import Test.Hspec

import AuthServer (runServer, retrieveUserHelper, fetchAllUsersHelper)
import Schema
import TestUtils (fromRight')

main :: IO ()
main = do
  tid <- forkIO runServer
  threadDelay 1000000
  hspec testSpec
  killThread tid

testSpec :: Spec
testSpec = describe "Basic Auth Tests" $ do
  it "Should fetch a user properly" $ do
    retrieveResult1 <- retrieveUserHelper 50 "Santa@Washington.com" "christmas"
    retrieveResult2 <- retrieveUserHelper 80 "Romaine@Trent.com" "lettuce"
    isRight retrieveResult1 `shouldBe` True
    (userName . fromRight' $ retrieveResult1) `shouldBe` "Santa Washington"
    isRight retrieveResult2 `shouldBe` True
    (userName . fromRight' $ retrieveResult2) `shouldBe` "Romaine Trent"
  it "Should not fetch when there's no/invalid password" $ do
    retrieveResult1 <- retrieveUserHelper 50 "Santa@Washington.com" "xmas"
    retrieveResult2 <- retrieveUserHelper 80 "Romaine@Trent.com" "kale"
    retrieveResult3 <- retrieveUserHelper 40 "Cheryl@Dahmer.com" ""
    isLeft retrieveResult1 `shouldBe` True
    isLeft retrieveResult2 `shouldBe` True
    isLeft retrieveResult3 `shouldBe` True
  it "Should only be able to fetch all users as admin" $ do
    fetchResult1 <- fetchAllUsersHelper "admin@test.com" "adminpassword"
    fetchResult2 <- fetchAllUsersHelper "Romaine@Trent.com" "lettuce"
    fetchResult3 <- fetchAllUsersHelper "Cheryl@Dahmer.com" ""
    isRight fetchResult1 `shouldBe` True
    isLeft fetchResult2 `shouldBe` True
    isLeft fetchResult3 `shouldBe` True
