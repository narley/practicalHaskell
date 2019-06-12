{-# LANGUAGE OverloadedStrings #-}

module DatabaseTestUtils where

import Control.Monad.Logger (LoggingT)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql (toSqlKey, Entity(..), SqlPersistT)

import Database (runAction, localConnString)
import Schema

-- Name Alias for runAction
rq :: SqlPersistT (LoggingT IO) a -> IO a
rq = runAction localConnString

mkComment :: (Int64, Int64, Int64, Integer, Text) -> Entity Comment
mkComment (cid, uid, aid, ts, body) = Entity (toSqlKey cid) $
  Comment body (toSqlKey uid) (toSqlKey aid) (posixSecondsToUTCTime (fromInteger ts))

-- Test Solutions (used in Lecture7Tests and Lecture8Tests)
trueComments1 :: [Entity Comment]
trueComments1 = mkComment <$>
  [ (10,2,145,1501906583,"I liked some of this. Other parts not so much.")
  , (147,2,154,1523320161,"The overall grammar and structure could be improved.")
  ]

trueComments2 :: [Entity Comment]
trueComments2 = mkComment <$>
  [ (105,5,147,1485210765,"I'm not sure I agree with your point about this.")
  , (88,5,158,1527994934,"You're certainly unique in your perspective on this.")
  , (131,5,175,1492690935,"Fascinating insights.")
  ]

trueComments3 :: [Entity Comment]
trueComments3 = mkComment <$>
  [ (179,37,120,1519876804,"This article was really great!")
  , (74,37,131,1487905272,"The overall grammar and structure could be improved.")
  , (166,37,135,1521271650,"Fascinating insights.")
  ]

trueComments4 :: [Entity Comment]
trueComments4 = []

trueComments5 :: [Entity Comment]
trueComments5 = mkComment <$>
  [ (123,98,99,1523420000,"This article was really great!")
  , (30,98,142,1492500779,"This article was really great!")
  ]

trueComments6 :: [Entity Comment]
trueComments6 = mkComment <$>
 [ (7,43,121,1511631434,"This is outstanding!")
 , (146,8,103,1544202846,"Did you consider the alternative argument posted today?")
 ]

trueComments7 :: [Entity Comment]
trueComments7 = []

trueComments8 :: [Entity Comment]
trueComments8 = mkComment <$>
 [ (134,29,177,1497522638,"This is outstanding!")
 , (23,86,177,1497768758,"I really liked this article!")
 , (135,52,107,1540104665,"The overall grammar and structure could be improved.")
 , (192,47,107,1540659764,"I liked some of this. Other parts not so much.")
 ]

trueComments9 :: [Entity Comment]
trueComments9 = mkComment <$>
 [ (73,70,181,1491478539,"The overall grammar and structure could be improved.")
 , (159,31,116,1516714147,"Thanks for writing this!")
 , (119,75,116,1516925272,"The overall grammar and structure could be improved.")
 , (139,79,116,1517016212,"There are a lot of ways to improve your writing.")
 , (31,32,116,1517158181,"Your site has some novel content.")
 , (99,42,168,1555832947,"Fascinating insights.")
 ]

trueComments10 :: [Entity Comment]
trueComments10 = mkComment <$>
  [ (68,79,91,1521793908,"I'm not sure I agree with your point about this.")
  , (132,35,91,1521855932,"The overall grammar and structure could be improved.")
  , (12,36,91,1522054636,"The overall grammar and structure could be improved.")
  ]
