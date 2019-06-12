module PGUtils where

import Database.PostgreSQL.Simple.FromRow as PG
import Database.SQLite.Simple.FromRow as SQLite

import BasicTypes (User(..), Article(..))

instance PG.FromRow User where
  fromRow = User <$> PG.field <*> PG.field <*> PG.field <*> PG.field

instance PG.FromRow Article where
  fromRow = Article <$> PG.field <*> PG.field <*> PG.field <*> PG.field

instance SQLite.FromRow User where
  fromRow = User <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field

instance SQLite.FromRow Article where
  fromRow = Article <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
