module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Lecture6
import Schema

main :: IO ()
main = defaultMain $ testGroup "Lecture 4 Tests" [specialPairsTests]

specialPairsTests :: TestTree
specialPairsTests = testCase "Special Pairs Tests" $ do
  pairs <- fetchSpecialPairs
  length pairs @?= 298
