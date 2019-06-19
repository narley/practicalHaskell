module Utils where

import Data.Char (toLower)

dropXAndLowerFirst :: Int -> String -> String
dropXAndLowerFirst x name = (toLower firstChar) : restChars
  where
    (firstChar : restChars) = drop x name
