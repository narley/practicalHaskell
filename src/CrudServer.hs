{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module CrudServer where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Database (runAction, localConnString)
import Database.Persist (Entity(..), insert, get, update, delete, Update, (=.))
import Database.Persist.Postgresql (ConnectionString, fromSqlKey, toSqlKey)
import Database.Esqueleto (select, from)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server


import Schema

type UsersCRUD = "users" :> CRUD User
-- type UsersCRUD = "users" :>
--   ( "all" :> Get '[JSON] [Entity User] :<|>
--     Capture "uid" Int64 :> Get '[JSON] (Maybe User) :<|>
--     "create" :> ReqBody '[JSON] User :> Post '[JSON] Int64 :<|>
--     "delete" :> Capture "uid" Int64 :> Delete '[JSON] () :<|>
--     "update" :> Capture "uid" Int64 :> ReqBody '[JSON] User :> Put '[JSON] ()
--   )

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

usersServer :: ConnectionString -> Server UsersCRUD
usersServer conn =
  getAllUsers conn :<|>
  retrieveUser conn :<|>
  createUser conn :<|>
  deleteUser conn :<|>
  updateUser conn

-- TODO Fill in these types, handler definitions, and add them to the Full API and Server at the bottom

type ArticlesCRUD = "articles" :> CRUD Article
-- type ArticlesCRUD = "articles" :>
--   ( "all" :> Get '[JSON] [Entity Article] :<|>
--     Capture "aid" Int64 :> Get '[JSON] (Maybe Article) :<|>
--     "create" :> ReqBody '[JSON] Article :> Post '[JSON] Int64 :<|>
--     "delete" :> Capture "aid" Int64 :> Delete '[JSON] () :<|>
--     "update" :> Capture "aid" Int64 :> ReqBody '[JSON] Article :> Put '[JSON] ()
--   )

getAllArticles :: ConnectionString -> Handler [Entity Article]
getAllArticles conn =
  liftIO $ runAction conn $ select . from $ \articles -> return articles

retrieveArticle :: ConnectionString -> Int64 -> Handler (Maybe Article)
retrieveArticle conn aid =
  liftIO $ runAction conn $ get (toSqlKey aid)

createArticle :: ConnectionString -> Article -> Handler Int64
createArticle conn article =
  fromSqlKey <$> liftIO (runAction conn $ insert article)

deleteArticle :: ConnectionString -> Int64 -> Handler ()
deleteArticle conn aid =
  liftIO $ runAction conn $ delete (toSqlKey aid :: Key Article)

updateArticle :: ConnectionString -> Int64 -> Article -> Handler ()
updateArticle conn aid article =
  liftIO $ runAction conn $ update (toSqlKey aid) articleUpdates
  where
    articleUpdates :: [Update Article]
    articleUpdates =
      [ ArticleTitle =. articleTitle article
      , ArticleBody =. articleBody article
      , ArticlePublishedAt =. articlePublishedAt article
      , ArticleAuthorId =. articleAuthorId article
      ]

articlesServer :: ConnectionString -> Server ArticlesCRUD
articlesServer conn =
  getAllArticles conn :<|>
  retrieveArticle conn :<|>
  createArticle conn :<|>
  deleteArticle conn :<|>
  updateArticle conn

type CommentsCRUD = "comments" :> CRUD Comment
-- type CommentsCRUD = "comments" :>
--   ( "all" :> Get '[JSON] [Entity Comment] :<|>
--     Capture "cid" Int64 :> Get '[JSON] (Maybe Comment) :<|>
--     "create" :> ReqBody '[JSON] Comment :> Post '[JSON] Int64 :<|>
--     "delete" :> Capture "cid" Int64 :> Delete '[JSON] () :<|>
--     "update" :> Capture "cid" Int64 :> ReqBody '[JSON] Comment :> Put '[JSON] ()
--   )

getAllComments :: ConnectionString -> Handler [Entity Comment]
getAllComments conn =
  liftIO $ runAction conn $ select . from $ \comments -> return comments

retrieveComment :: ConnectionString -> Int64 -> Handler (Maybe Comment)
retrieveComment conn cid =
  liftIO $ runAction conn $ get (toSqlKey cid)

createComment :: ConnectionString -> Comment -> Handler Int64
createComment conn comment =
  fromSqlKey <$> liftIO (runAction conn $ insert comment)

deleteComment :: ConnectionString -> Int64 -> Handler ()
deleteComment conn cid =
  liftIO $ runAction conn $ delete (toSqlKey cid :: Key Comment)

updateComment :: ConnectionString -> Int64 -> Comment -> Handler ()
updateComment conn cid comment =
  liftIO $ runAction conn $ update (toSqlKey cid) commentUpdates
  where
    commentUpdates :: [Update Comment]
    commentUpdates =
      [ CommentUserId =. commentUserId comment
      , CommentArticleId =. commentArticleId comment
      , CommentBody =. commentBody comment
      , CommentSubmittedAt =. commentSubmittedAt comment
      ]

commentsServer :: ConnectionString -> Server CommentsCRUD
commentsServer conn =
  getAllComments conn :<|>
  retrieveComment conn :<|>
  createComment conn :<|>
  deleteComment conn :<|>
  updateComment conn

-- TODO Add your new CRUD types to this type!
type FullCRUD = UsersCRUD :<|> ArticlesCRUD :<|> CommentsCRUD

fullCRUD :: Proxy FullCRUD
fullCRUD = Proxy

fullServer :: ConnectionString -> Server FullCRUD
fullServer conn =
  usersServer conn :<|>
  articlesServer conn :<|>
  commentsServer conn

runServer :: IO ()
runServer = run 8080 (serve fullCRUD (fullServer localConnString))

-- TODO Parameterize the idea of a CRUD type!
type CRUD a =
  ( "all" :> Get '[JSON] [Entity a] :<|>
    Capture "uid" Int64 :> Get '[JSON] (Maybe a) :<|>
    "create" :> ReqBody '[JSON] a :> Post '[JSON] Int64 :<|>
    "delete" :> Capture "uid" Int64 :> Delete '[JSON] () :<|>
    "update" :> Capture "uid" Int64 :> ReqBody '[JSON] a :> Put '[JSON] ()
  )
