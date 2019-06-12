module BasicTypes where

import Data.Time (UTCTime)

data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  , userAge :: Int
  }

data Article = Article
  { articleId :: Int
  , articleTitle :: String
  , articleBody :: String
  , articlePublishedAt :: UTCTime
  }
