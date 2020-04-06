{-# LANGUAGE OverloadedStrings #-}

module Lecture6 where

import Control.Monad (forM_)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, LogLevel(..), LogSource, filterLogger)
import Control.Monad.Reader (runReaderT)
import Data.List (sortBy)
import Database.Persist (Entity(..), (<.), (>.), (==.), selectList, Filter(..), SelectOpt(..))
import Database.Persist.Postgresql (withPostgresqlConn, ConnectionString, SqlPersistT, fromSqlKey)
import qualified Data.Text as T

import Database (localConnString)
import Schema

import Data.List

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction conn action = runStdoutLoggingT . filterLogger logFilter $
  withPostgresqlConn conn (runReaderT action)

logFilter :: LogSource -> LogLevel -> Bool
logFilter _ LevelInfo = True
logFilter _ LevelWarn = True
logFilter _ LevelError = True
logFilter _ _ = False

fetchSpecialUsers :: IO ()
fetchSpecialUsers = do
  users <- runAction localConnString query
  forM_ users print
  where
    query :: SqlPersistT (LoggingT IO) [Entity User]
    query = selectList
      [UserName <. "N", UserName >. "C", UserAge <. 25, UserAge >. 20]
      [Asc UserAge]

fetchSpecialPairs :: IO [(Entity User, Entity Article)]
fetchSpecialPairs = runAction localConnString $ do
  allUsers <- usersQuery
  allArticles <- articlesQuery
  let allCombos = do
        u <- allUsers
        a <- filter (\a -> T.head (userName . entityVal $ u) == T.head (articleTitle . entityVal $ a)) allArticles
        return (u, a)
  return $ sortBy compareIds allCombos
  where
    usersQuery :: SqlPersistT (LoggingT IO) [Entity User]
    usersQuery = selectList [] []
    articlesQuery :: SqlPersistT (LoggingT IO) [Entity Article]
    articlesQuery = selectList [] []
    compareIds :: (Entity User, Entity Article) -> (Entity User, Entity Article) -> Ordering
    compareIds ((Entity uid1 _), _) ((Entity uid2 _), _) = compare (fromSqlKey uid1) (fromSqlKey uid2)
