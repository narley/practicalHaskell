{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.SQLite.Simple as SQLite
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import BasicTypes (User(..), Article(..))
import PGUtils ()

main :: IO ()
main = do
  pgConn <- PG.connectPostgreSQL "host='localhost' port=5432 dbname='mmh' user='postgres' password='postgres'"
  sqliteConn <- SQLite.open "local-dbs/lecture-1-2.db"
  defaultMain $ testGroup "Lecture 2 Tests"
    [ usersPGTest pgConn
    , articlesPGTest pgConn
    , usersSQLiteTest sqliteConn
    , articlesSQLiteTest sqliteConn
    ]

usersPGTest :: PG.Connection -> TestTree
usersPGTest pgConn = testCase "Users Tests (Postgres)" $ do
  (userResults :: [User]) <- PG.query_ pgConn "SELECT * FROM users;"
  [(PG.Only email1) :: PG.Only String] <- PG.query_ pgConn "SELECT email FROM users WHERE name = 'Chris';"
  [(PG.Only email2) :: PG.Only String] <- PG.query_ pgConn "SELECT email FROM users WHERE name = 'Kathleen';"
  [(PG.Only email3) :: PG.Only String] <- PG.query_ pgConn "SELECT email FROM users WHERE name = 'Michael';"
  length userResults @?= 3
  email1 @?= "chris@test.com"
  email2 @?= "kathleen@corp.com"
  email3 @?= "michael@college.edu"
  inserted <- PG.execute_ pgConn "INSERT INTO users VALUES (99, 'Timothy', 'tim@fun.com', 36);"
  deleted <- PG.execute_ pgConn "DELETE FROM users WHERE id = 99;"
  inserted @?= 1
  deleted @?= 1

articlesPGTest :: PG.Connection -> TestTree
articlesPGTest pgConn = testCase "Articles Tests (Postgres)" $ do
  (articleResults :: [Article]) <- PG.query_ pgConn "SELECT * FROM articles;"
  (a1 :: [Article]) <- PG.query_ pgConn "SELECT * FROM articles WHERE title = 'Introduction to Haskell';"
  (a2 :: [Article]) <- PG.query_ pgConn "SELECT * FROM articles WHERE title = 'Databases in Haskell';"
  (a3 :: [Article]) <- PG.query_ pgConn "SELECT * FROM articles WHERE title = 'Production Quality Systems';"
  length articleResults @?= 3
  length a1 @?= 1
  length a2 @?= 1
  length a3 @?= 1
  inserted <- PG.execute_ pgConn "INSERT INTO articles VALUES (99, 'An article', 'The text', '2018-12-15T12:00:00');"
  deleted <- PG.execute_ pgConn "DELETE FROM articles WHERE id = 99;"
  inserted @?= 1
  deleted @?= 1

usersSQLiteTest :: SQLite.Connection -> TestTree
usersSQLiteTest sqliteConn = testCase "Users Tests (SQLite)" $ do
  (userResults :: [User]) <- SQLite.query_ sqliteConn "SELECT * FROM users;"
  [(SQLite.Only email1) :: SQLite.Only String] <- SQLite.query_ sqliteConn "SELECT email FROM users WHERE name = 'Chris';"
  [(SQLite.Only email2) :: SQLite.Only String] <- SQLite.query_ sqliteConn "SELECT email FROM users WHERE name = 'Kathleen';"
  [(SQLite.Only email3) :: SQLite.Only String] <- SQLite.query_ sqliteConn "SELECT email FROM users WHERE name = 'Michael';"
  length userResults @?= 3
  email1 @?= "chris@test.com"
  email2 @?= "kathleen@corp.com"
  email3 @?= "michael@college.edu"
  SQLite.execute_ sqliteConn "INSERT INTO users VALUES (99, 'Timothy', 'tim@fun.com', 36);"
  SQLite.execute_ sqliteConn "DELETE FROM users WHERE id = 99;"

articlesSQLiteTest :: SQLite.Connection -> TestTree
articlesSQLiteTest sqliteConn = testCase "Articles Tests (SQLite)" $ do
  (articleResults :: [Article]) <- SQLite.query_ sqliteConn "SELECT * FROM articles;"
  (a1 :: [Article]) <- SQLite.query_ sqliteConn "SELECT * FROM articles WHERE title = 'Introduction to Haskell';"
  (a2 :: [Article]) <- SQLite.query_ sqliteConn "SELECT * FROM articles WHERE title = 'Databases in Haskell';"
  (a3 :: [Article]) <- SQLite.query_ sqliteConn "SELECT * FROM articles WHERE title = 'Production Quality Systems';"
  length articleResults @?= 3
  length a1 @?= 1
  length a2 @?= 1
  length a3 @?= 1
  SQLite.execute_ sqliteConn "INSERT INTO articles VALUES (99, 'An article', 'The text', '2018-12-15T12:00:00');"
  SQLite.execute_ sqliteConn "DELETE FROM articles WHERE id = 99;"
