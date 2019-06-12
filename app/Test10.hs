module Main where

import Control.Monad (void)
import System.Process (runCommand)

main :: IO ()
main = void $ runCommand "stack build PracticalHaskell:test:lecture-10-tests"
