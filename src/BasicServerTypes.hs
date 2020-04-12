module BasicServerTypes where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import Web.HttpApiData

newtype BasicServerUserId = BasicServerUserId Int64
  deriving (Show, Eq)

instance ToHttpApiData BasicServerUserId where
  toUrlPiece (BasicServerUserId uid) = pack $ show uid

instance FromHttpApiData BasicServerUserId where
  parseUrlPiece urlPiece = Right (BasicServerUserId $ read $ unpack urlPiece)

newtype DescriptionResponse = DescriptionResponse Text
  deriving (Show, Eq)

instance ToJSON DescriptionResponse where
  toJSON (DescriptionResponse r) = String r

instance FromJSON DescriptionResponse where
  parseJSON = withText "DescriptionResponse" $ \t ->
    return $ DescriptionResponse t
