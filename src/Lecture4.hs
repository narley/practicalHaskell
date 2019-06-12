module Lecture4 where

import Database.Persist (Entity(..), selectList)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import System.IO (Handle, hPrint)

import Database (runAction, localConnString)
import Schema

myUser :: User
myUser = undefined

-- TODO UNCOMMENT ME!
-- myArticle :: Article
-- myArticle = undefined

myUserEntity :: Entity User
myUserEntity = undefined

-- TODO UNCOMMENT ME!
-- myArticleEntity :: Entity Article
-- myArticleEntity = undefined

fetchAllUsers :: IO [Entity User]
fetchAllUsers = runAction localConnString $ selectList [] []

printAllKeys :: Handle -> IO ()
printAllKeys = undefined
