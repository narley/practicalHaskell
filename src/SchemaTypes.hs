{-# LANGUAGE TemplateHaskell #-}
module SchemaTypes where

import Data.Aeson.TH
import Data.Time (UTCTime)
import Database.Persist.TH

data ReactionType = Like | Love | Dislike
  deriving (Show, Read, Eq)

data Metadata = Metadata
  { reactionTime :: UTCTime
  , previousLikes :: Int
  , previousLoves :: Int
  , previousDislikes :: Int
  }
  deriving (Show, Read, Eq)

deriveJSON defaultOptions ''ReactionType
deriveJSON defaultOptions ''Metadata

derivePersistFieldJSON "ReactionType"

derivePersistFieldJSON "Metadata"
