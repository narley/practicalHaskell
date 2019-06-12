{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad.Logger (runStdoutLoggingT, LoggingT, LogLevel(..), filterLogger)
import Control.Monad.Reader (runReaderT)
import Database.Persist.Postgresql (runMigration, ConnectionString, withPostgresqlConn, SqlPersistT)

import Schema (migrateAll)

localConnString :: ConnectionString
localConnString = "host='localhost' port=5432 dbname='mmh' user='postgres' password='postgres'"

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connString action =
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connString $ \backend ->
    runReaderT action backend

-- You can safely ignore this code!
logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug = False
logFilter _ (LevelOther _) = False
