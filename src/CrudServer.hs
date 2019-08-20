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

type UsersCRUD = "users" :>
  ( "all" :> Get '[JSON] [Entity User] :<|>
    Capture "uid" Int64 :> Get '[JSON] (Maybe User) :<|>
    "create" :> ReqBody '[JSON] User :> Post '[JSON] Int64 :<|>
    "delete" :> Capture "uid" Int64 :> Delete '[JSON] () :<|>
    "update" :> Capture "uid" Int64 :> ReqBody '[JSON] User :> Put '[JSON] ()
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

usersServer :: ConnectionString -> Server UsersCRUD
usersServer conn =
  getAllUsers conn :<|>
  retrieveUser conn :<|>
  createUser conn :<|>
  deleteUser conn :<|>
  updateUser conn

-- TODO Fill in these types, handler definitions, and add them to the Full API and Server at the bottom

-- type ArticlesCRUD = ...
getAllArticles :: ConnectionString -> Handler [Entity Article]
getAllArticles conn = undefined

retrieveArticle :: ConnectionString -> Int64 -> Handler (Maybe Article)
retrieveArticle conn = undefined

createArticle :: ConnectionString -> Article -> Handler Int64
createArticle conn = undefined

deleteArticle :: ConnectionString -> Int64 -> Handler ()
deleteArticle conn = undefined

updateArticle :: ConnectionString -> Int64 -> Article -> Handler ()
updateArticle conn = undefined

-- articlesServer :: ConnectionString -> Server ArticlesCRUD

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
type FullCRUD = UsersCRUD

fullCRUD :: Proxy FullCRUD
fullCRUD = Proxy

fullServer :: ConnectionString -> Server FullCRUD
fullServer = usersServer

runServer :: IO ()
runServer = run 8080 (serve fullCRUD (fullServer localConnString))

-- TODO Parameterize the idea of a CRUD type!
-- type CRUD = ???
