module BasicServerTypes where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Web.HttpApiData

newtype BasicServerUserId = BasicServerUserId Int64

instance FromHttpApiData BasicServerUserId where
  parseUrlPiece = undefined

newtype DescriptionResponse = DescriptionResponse Text

instance ToJSON DescriptionResponse where
  toJSON = undefined
