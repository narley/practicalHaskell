{-# LANGUAGE OverloadedStrings #-}

module Lecture6 where

import Control.Monad (forM_)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, LogLevel(..), filterLogger)
import Control.Monad.Reader (runReaderT)
import Data.List (sortBy)
import Database.Persist (Entity(..), (<.), (>.), (==.), selectList, Filter(..), SelectOpt(..))
import Database.Persist.Postgresql (withPostgresqlConn, ConnectionString, SqlPersistT, fromSqlKey)
import qualified Data.Text as T

import Database (localConnString)
import Schema

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction conn action = runStdoutLoggingT $
  withPostgresqlConn conn (runReaderT action)

-- TODO: UNCOMMENT ME!
-- logFilter :: ???
-- logFilter = undefined

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
fetchSpecialPairs = undefined
