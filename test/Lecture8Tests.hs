{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import qualified Data.Text as T
import Database (runAction, localConnString)
import Database.Persist (deleteWhere, Filter)
import Data.Either (isRight, isLeft)
import Test.Hspec

import AdvancedAuthServer (runServer, retrieveUserHelper, fetchAllUsersHelper, loginHelper, logoutHelper)
import Schema
import TestUtils (fromRight')

main :: IO ()
main = do
  tid <- forkIO runServer
  threadDelay 1000000
  hspec (beforeAll_ deleteCookies (afterAll_ deleteCookies testSpec))
  killThread tid

-- Fetch user (fails)
-- Login
-- Fetch Right User (succeeds)
-- Fetch wrong user (fails)
-- Fetch all users (fails)
-- Logout

deleteCookies :: IO ()
deleteCookies = runAction localConnString $ deleteWhere ([] :: [Filter LoginToken])

testSpec :: Spec
testSpec = describe "Advanced Auth Tests" $ do
  it "Should run a normal user session properly (fetch, login, fetch, logout)" $ do
    retrieveResult1 <- retrieveUserHelper 50 "christmas"
    isLeft retrieveResult1 `shouldBe` True
    loginResult <- loginHelper "Santa@Washington.com" "christmas"
    isRight loginResult `shouldBe` True
    let (uid, cookie) = fromRight' loginResult
    uid `shouldBe` 50
    retrieveResult2 <- retrieveUserHelper 50 (T.unpack cookie)
    isRight retrieveResult2 `shouldBe` True
    let user = fromRight' retrieveResult2
    retrieveResult3 <- retrieveUserHelper 51 (T.unpack cookie)
    isLeft retrieveResult3 `shouldBe` True
    allResult <- fetchAllUsersHelper (T.unpack cookie)
    isLeft allResult `shouldBe` True
    logoutResult <- logoutHelper (T.unpack cookie)
    isRight logoutResult `shouldBe` True
  it "Should be able to fetch all users as admin" $ do
    loginResult1 <- loginHelper "admin@test.com" "password"
    isLeft loginResult1 `shouldBe` True
    loginResult2 <- loginHelper "admin@test.com" "adminpassword"
    isRight loginResult2 `shouldBe` True
    let (_, cookie) = fromRight' loginResult2
    allResult <- fetchAllUsersHelper (T.unpack cookie)
    isRight allResult `shouldBe` True
    logoutResult <- logoutHelper (T.unpack cookie)
    isRight logoutResult `shouldBe` True
