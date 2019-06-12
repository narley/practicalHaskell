{-# LANGUAGE TemplateHaskell #-}
module SchemaTypes where

import Data.Time (UTCTime)
import Database.Persist.TH

data ReactionType = Like | Love | Dislike
  deriving (Show, Read, Eq)
