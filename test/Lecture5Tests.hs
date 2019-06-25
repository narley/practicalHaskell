module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "Lecture 5 Tests"
  []
