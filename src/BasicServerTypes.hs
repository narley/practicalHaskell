module BasicServerTypes where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Web.HttpApiData

newtype BasicServerUserId = BasicServerUserId Int64
  deriving (Show, Eq)

instance ToHttpApiData BasicServerUserId where
  toUrlPiece (BasicServerUserId uid) = undefined

instance FromHttpApiData BasicServerUserId where
  parseUrlPiece urlPiece = undefined

newtype DescriptionResponse = DescriptionResponse Text
  deriving (Show, Eq)

instance ToJSON DescriptionResponse where
  toJSON (DescriptionResponse r) = undefined

instance FromJSON DescriptionResponse where
  parseJSON = undefined
