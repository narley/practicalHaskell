module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = do
  defaultMain $ testGroup "Lecture 2 Tests"
    [ testTemplate
    ]

testTemplate :: TestTree
testTemplate = testCase "Template" $ do
  True @?= True
