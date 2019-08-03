{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Either (isRight)
import Test.Hspec

import FinalServer (runServer, helloHelper)
import TestUtils (fromRight')

main :: IO ()
main = do
  tid <- forkIO runServer
  threadDelay 1000000
  hspec testSpec
  killThread tid

testSpec :: Spec
testSpec = describe "Final Server Tests" $
  it "Hit hello endpoint" $ do
    result <- helloHelper
    isRight result `shouldBe` True
    fromRight' result `shouldBe` "Hello Haskell Heroku"
