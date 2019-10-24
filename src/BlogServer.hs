{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module BlogServer where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.PasswordStore (verifyPassword)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database (runAction, localConnString)
import Database.Persist (Entity(..), insert, get, update, delete, Update, (=.))
import Database.Persist.Postgresql (ConnectionString, fromSqlKey, toSqlKey)
import Database.Esqueleto (select, from, orderBy, desc, limit, (^.), where_, (==.), val)
import Network.HTTP.Media ((//), (/:))
import Network.Wai (ResponseReceived, Response, mapResponseHeaders)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles (serveDirectoryWebApp)


import JWTHelpers (makeJWTCookie)
import Schema

type UsersCRUD = "api" :> "users" :>
  ( "all" :> Get '[JSON] [Entity User] :<|>
    Capture "uid" Int64 :> Get '[JSON] (Maybe User) :<|>
    "create" :> ReqBody '[JSON] User :> Post '[JSON] Int64 :<|>
    "delete" :> Capture "uid" Int64 :> Delete '[JSON] () :<|>
    "update" :> Capture "uid" Int64 :> ReqBody '[JSON] User :> Put '[JSON] () :<|>
    "login" :> ReqBody '[JSON] LoginInfo :> Post '[JSON] LoginResponse
  )

getAllUsers :: ConnectionString -> Handler [Entity User]
getAllUsers conn = liftIO $ runAction conn $ select . from $ \users -> return users

retrieveUser :: ConnectionString -> Int64 -> Handler (Maybe User)
retrieveUser conn uid = liftIO $ runAction conn $ get (toSqlKey uid)

createUser :: ConnectionString -> User -> Handler Int64
createUser conn user = fromSqlKey <$> liftIO (runAction conn $ insert user)

deleteUser :: ConnectionString -> Int64 -> Handler ()
deleteUser conn uid = liftIO (runAction conn $ delete (toSqlKey uid :: Key User))

updateUser :: ConnectionString -> Int64 -> User -> Handler ()
updateUser conn uid user = liftIO $ runAction conn $ update (toSqlKey uid) userUpdates
  where
    userUpdates :: [Update User]
    userUpdates =
      [ UserName =. userName user
      , UserEmail =. userEmail user
      , UserAge =. userAge user
      ]

loginUser :: ConnectionString -> LoginInfo -> Handler LoginResponse
loginUser conn (LoginInfo username password) = do
  userEntityList <- liftIO $ runAction conn $ select . from $ \users -> do
    where_ (users ^. UserEmail ==. val username)
    return users
  case userEntityList of
    [(Entity uid _)] -> do
      authDataE <- liftIO $ runAction conn $ select . from $ \authData -> do
        where_ (authData ^. AuthDataUserId ==. val uid)
        return authData
      case authDataE of
        [(Entity _ authData)] -> if verifyPassword (encodeUtf8 password) (authDataHashString authData)
          then do
            let cookie = makeJWTCookie (fromSqlKey uid)
            return $ LoginResponse (fromSqlKey uid) cookie
          else throwError $ err403 { errBody = "Invalid Password!"}
        _ -> throwError $ err403 { errBody = "No Auth Data"}
    _ -> throwError $ err403 { errBody = "Invalid user!"}

usersServer :: ConnectionString -> Server UsersCRUD
usersServer conn =
  getAllUsers conn :<|>
  retrieveUser conn :<|>
  createUser conn :<|>
  deleteUser conn :<|>
  updateUser conn :<|>
  loginUser conn

-- TODO Fill in these types, handler definitions, and add them to the Full API and Server at the bottom

type ArticlesCRUD = "api" :> "articles" :>
  ( "all" :> Get '[JSON] [Entity Article] :<|>
    Capture "uid" Int64 :> Get '[JSON] (Maybe Article) :<|>
    "create" :> ReqBody '[JSON] Article :> Post '[JSON] Int64 :<|>
    "delete" :> Capture "uid" Int64 :> Delete '[JSON] () :<|>
    "update" :> Capture "uid" Int64 :> ReqBody '[JSON] Article :> Put '[JSON] () :<|>
    "newest" :> QueryParam "limit" Int64 :> Get '[JSON] [Entity Article]
  )

getAllArticles :: ConnectionString -> Handler [Entity Article]
getAllArticles conn = liftIO $ runAction conn $ select . from $ \articles -> return articles

retrieveArticle :: ConnectionString -> Int64 -> Handler (Maybe Article)
retrieveArticle conn aid = liftIO $ runAction conn $ get (toSqlKey aid)

createArticle :: ConnectionString -> Article -> Handler Int64
createArticle conn article = fromSqlKey <$> liftIO (runAction conn $ insert article)

deleteArticle :: ConnectionString -> Int64 -> Handler ()
deleteArticle conn aid = liftIO (runAction conn $ delete (toSqlKey aid :: Key Article))

updateArticle :: ConnectionString -> Int64 -> Article -> Handler ()
updateArticle conn aid article = liftIO $ runAction conn $ update (toSqlKey aid) articleUpdates
  where
    articleUpdates :: [Update Article]
    articleUpdates =
      [ ArticleTitle =. articleTitle article
      , ArticleBody =. articleBody article
      , ArticlePublishedAt =. articlePublishedAt article
      , ArticleAuthorId =. articleAuthorId article
      ]

newestArticles :: ConnectionString -> Maybe Int64 -> Handler [Entity Article]
newestArticles conn articleLimit = liftIO $ runAction conn $ select . from $ \articles -> do
  orderBy [desc (articles ^. ArticlePublishedAt)]
  limit (fromMaybe 3 articleLimit)
  return articles

articlesServer :: ConnectionString -> Server ArticlesCRUD
articlesServer conn =
  getAllArticles conn :<|>
  retrieveArticle conn :<|>
  createArticle conn :<|>
  deleteArticle conn :<|>
  updateArticle conn :<|>
  newestArticles conn

-- type CommentsCRUD = ...
getAllComments :: ConnectionString -> Handler [Entity Comment]
getAllComments conn = undefined

retrieveComment :: ConnectionString -> Int64 -> Handler (Maybe Comment)
retrieveComment conn = undefined

createComment :: ConnectionString -> Comment -> Handler Int64
createComment conn = undefined

deleteComment :: ConnectionString -> Int64 -> Handler ()
deleteComment conn = undefined

updateComment :: ConnectionString -> Int64 -> Comment -> Handler ()
updateComment conn = undefined

-- commentsServer :: ConnectionString -> Server CommentsCRUD

-- TODO Add your new CRUD types to this type!
type FullCRUD = UsersCRUD :<|> ArticlesCRUD

fullCRUD :: Proxy FullCRUD
fullCRUD = Proxy

fullServer :: ConnectionString -> Server FullCRUD
fullServer conn = usersServer conn :<|> articlesServer conn


type StaticContentApi =
  "static" :> Raw :<|>
  "login" :> Get '[HTML] RawHtml :<|>
  "blog" :> Capture "article_id" Int64 :> Get '[HTML] RawHtml :<|>
  Get '[HTML] RawHtml

staticServer :: Server StaticContentApi
staticServer =
  serveDirectoryWebApp "frontend/static" :<|>
  loadIndex :<|>
  (\_ -> loadIndex) :<|>
  loadIndex

staticApi :: Proxy StaticContentApi
staticApi = Proxy

loadIndex :: Handler RawHtml
loadIndex = RawHtml <$> (liftIO $ BSL.readFile "frontend/index.html")

data HTML

newtype RawHtml = RawHtml { unRaw :: BSL.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type BlogApi = FullCRUD :<|> StaticContentApi

blogApi :: Proxy BlogApi
blogApi = Proxy

blogServer :: ConnectionString -> Server BlogApi
blogServer conn = fullServer conn :<|> staticServer

addElmMiddleware :: Application -> Application
addElmMiddleware baseApp req responseFunc = baseApp req newResponseFunc
  where
    newResponseFunc :: Response -> IO ResponseReceived
    newResponseFunc = responseFunc . (addOriginsAllowed . addHeadersAllowed)

addOriginsAllowed :: Response -> Response
addOriginsAllowed = mapResponseHeaders $
  (:) ("Access-Control-Allow-Origin", "*")

addHeadersAllowed :: Response -> Response
addHeadersAllowed = mapResponseHeaders $
  (:) ("Access-Control-Allow-Headers", "content-type")

runServer :: IO ()
runServer = run 8080 ((addElmMiddleware . (provideOptions fullCRUD)) (serve blogApi (blogServer localConnString)))

{- MODIFICATIONS FOR ELM
 -
 - By default, Elm does a few things with HTTP requests around Access Control,
 - or Control Origin Resource Sharing (CORS).
 -
 - To make our Servant server compatible with Elm, we need to do a few things.
 -
 - 1. We need to allow all origins for access control. This means modifying all
 -    requests with the header "Access-Control-Allow-Origin": "*"
 - 2. We need to allow content type as a header field. For this, we add another header to each request:
 -    "Access-Control-Allow-Headers": "content-type"
 -
 - We add both these using "Middleware", a layer that sits on our application and can modify the
 - incoming requests. See the expressions "addElmMiddleware", "addOriginsAllowed" and "addHeadersAllowed"
 -
 - 3. We also need to permit requests with the type "Options". When posting data, Elm sends
 -    "pre-flight" options requests to check the endpoint for validity. We add these to our API using
 -    the "provideOptions" function that you see in our final server definition.
 -}
