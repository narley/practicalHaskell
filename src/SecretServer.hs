{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SecretServer where

import qualified Data.Aeson as Ae
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.UUID as U
import Network.HTTP.Types.Status (Status(..))
import System.Random
import Web.Spock
import Web.Spock.Config

data EmptySession = EmptySession
data AppState = AppState U.UUID U.UUID

data AuthData = AuthData Text Text

instance Ae.FromJSON AuthData where
  parseJSON = Ae.withObject "Auth Data" $ \o -> do
    uName <- o Ae..: "username"
    pass <- o Ae..: "password"
    return $ AuthData uName pass

runSecretServer :: Maybe Int -> IO ()
runSecretServer seed = do
  gen <- case seed of
    Nothing -> getStdGen
    Just seedVal -> return $ mkStdGen seedVal
  let (token, gen2) = random gen
  let (secret, _) = random gen2
  spockConfig <- defaultSpockCfg EmptySession PCNoDatabase (AppState token secret)
  runSpock 8081 (spock spockConfig app)

app :: SpockM () EmptySession AppState ()
app = do
  post "auth" $ do
    (authData :: Maybe AuthData) <- Ae.decode . fromStrict <$> body
    case authData of
      Nothing -> do
        setStatus (Status 403 "Auth data format is invalid")
        json ("Auth data format is invalid" :: Text)
      Just (AuthData username password) -> if username == trueUsername && password == truePassword
        then do
          (AppState apiToken _) <- getState
          json (U.toText apiToken)
        else do
          setStatus (Status 403 "Incorrect auth data")
          json ("Incorrect auth data" :: Text)
  get ("secret" <//> var) $ \token -> do
    (AppState apiToken secret) <- getState
    if token == U.toText apiToken
      then json (U.toText secret)
      else do
        setStatus (Status 403 "Invalid API token")
        json ("Invalid API Token" :: Text)

trueUsername :: Text
trueUsername = "test"

truePassword :: Text
truePassword = "pass"
