{-# LANGUAGE OverloadedStrings #-}
module JWTHelpers where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Map as Map

import Web.JWT

-- This isn't good! You should, at the very least, use an environment variable
-- for this secret value. But better yet, use a binarySecret with a private key file
serverSecret :: Secret
serverSecret = secret "monday-morning-haskell"

makeJWTCookie :: Int64 -> Text
makeJWTCookie uid = encodeSigned HS256 serverSecret claimsMap
  where
    claimsMap = def { unregisteredClaims = Map.fromList [("user_id", toJSON uid)] }

decodeJWTCookie :: Text -> Maybe Int64
decodeJWTCookie cookie = do
  verifiedJWT <- decodeAndVerifySignature serverSecret cookie
  uidValue <- Map.lookup "user_id" (unregisteredClaims . claims $ verifiedJWT)
  case fromJSON uidValue of
    Success a -> Just a
    _ -> Nothing
