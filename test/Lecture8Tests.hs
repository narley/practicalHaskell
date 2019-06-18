{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sortBy)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Text (Text)
import Database.Persist.Sql (toSqlKey, Entity(..))
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Lecture8
import Database (runAction, localConnString)
import Schema

import DatabaseTestUtils

main :: IO ()
main = defaultMain $ testGroup "Lecture 7 Tests"
  [ lastYearsArticlesTest
  , getYoungUsersTest
  , getSpecialPairsTest
  , commentsFromUserTests
  , commentsOnUserTests
  ]

mkUser :: (Int64, Text, Text, Int) -> Entity User
mkUser (uid, name, email, age) = Entity (toSqlKey uid) $
  User name email age

mkArticle :: (Int64, Text, Text, Int64, Integer) -> Entity Article
mkArticle (aid, body, title, uid, ts) = Entity (toSqlKey aid) $
  Article title body (posixSecondsToUTCTime (fromInteger ts)) (toSqlKey uid)

lastYearsArticlesTest :: TestTree
lastYearsArticlesTest = testCase "Last Years Articles Test" $ do
  articles <- rq lastYearsArticles
  let trueArticles = sortBy sortByTitle expectedArticles
  length articles @?= length trueArticles
  mapM_ (\(a1, a2) -> entityKey a1 @?= entityKey a2) (zip articles trueArticles)
  where
    sortByTitle :: Entity Article -> Entity Article -> Ordering
    sortByTitle (Entity _ a1) (Entity _ a2) = compare (articleTitle a1) (articleTitle a2)

expectedArticles :: [Entity Article]
expectedArticles = mkArticle <$>
   [ (87,"WRITING CODE Is Essential For Your Success. Read This To Find Out Why","You Don't Have To Be A Big Corporation To Start PROGRAMMING HASKELL",96,1543168982)
   , (89,"If WRITING CODE Is So Terrible, Why Don't Statistics Show It?","Are You Making These PROGRAMMING HASKELL Mistakes?",28,1535033037)
   , (90,"World Class Tools Make WRITING CODE Push Button Easy","Here Is A Quick Cure For PROGRAMMING HASKELL",36,1542730207)
   , (91,"WRITING CODE Strategies For Beginners","What Everyone Ought To Know About PROGRAMMING HASKELL",66,1521710714)
   , (98,"The Best Way To WRITING CODE","PROGRAMMING HASKELL Your Way To Success",62,1545701109)
   , (99,"WRITING CODE Made Simple - Even Your Kids Can Do It","5 Brilliant Ways To Use PROGRAMMING HASKELL",74,1523087590)
   , (100,"WRITING CODE? It's Easy If You Do It Smart","Boost Your PROGRAMMING HASKELL With These Tips",18,1525886329)
   , (103,"15 Tips For WRITING CODE Success","Where Is The Best PROGRAMMING HASKELL?",4,1543960283)
   , (105,"Now You Can Have The WRITING CODE Of Your Dreams √ Cheaper/Faster Than You Ever Imagined","How To Turn PROGRAMMING HASKELL Into Success",94,1525313575)
   , (107,"These 5 Simple WRITING CODE Tricks Will Pump Up Your Sales Almost Instantly","The Truth About PROGRAMMING HASKELL In 3 Minutes",16,1540065798)
   , (108,"WRITING CODE Works Only Under These Conditions","Essential PROGRAMMING HASKELL Smartphone Apps",95,1544438329)
   , (110,"WRITING CODE Is Bound To Make An Impact In Your Business","How To Win Clients And Influence Markets with PROGRAMMING HASKELL",12,1533366591)
   , (112,"3 Ways Twitter Destroyed My WRITING CODE Without Me Noticing","Are You Embarrassed By Your PROGRAMMING HASKELL Skills? Here's What To Do",45,1545211571)
   , (113,"5 Simple Steps To An Effective WRITING CODE Strategy","What Zombies Can Teach You About PROGRAMMING HASKELL",71,1515726153)
   , (116,"Revolutionize Your WRITING CODE With These Easy-peasy Tips","PROGRAMMING HASKELL An Incredibly Easy Method That Works For All",32,1516660025)
   , (117,"Congratulations! Your WRITING CODE Is (Are) About To Stop Being Relevant","Have You Heard? PROGRAMMING HASKELL Is Your Best Bet To Grow",56,1523007300)
   , (118,"Where Is The Best WRITING CODE?","10 Warning Signs Of Your PROGRAMMING HASKELL Demise",26,1543666616)
   , (120,"The Philosophy Of WRITING CODE","5 Simple Steps To An Effective PROGRAMMING HASKELL Strategy",49,1519600863)
   , (123,"5 Reasons WRITING CODE Is A Waste Of Time","The Ultimate Deal On PROGRAMMING HASKELL",69,1523940677)
   , (127,"Is WRITING CODE Worth [$] To You?","Double Your Profit With These 5 Tips on PROGRAMMING HASKELL",22,1515400272)
   , (132,"Being A Star In Your Industry Is A Matter Of WRITING CODE","10 Best Practices For PROGRAMMING HASKELL",85,1538391044)
   , (135,"WRITING CODE And Love Have 4 Things In Common","How To Earn $398/Day Using PROGRAMMING HASKELL",68,1521134918)
   , (136,"In 10 Minutes, I'll Give You The Truth About WRITING CODE","PROGRAMMING HASKELL Works Only Under These Conditions",77,1530444656)
   , (139,"Fear? Not If You Use WRITING CODE The Right Way!","Can You Really Find PROGRAMMING HASKELL (on the Web)?",45,1543531456)
   , (148,"3 Ways To Have (A) More Appealing WRITING CODE","Everything You Wanted to Know About PROGRAMMING HASKELL and Were Afraid To Ask",17,1528206529)
   , (150,"Here Is A Quick Cure For WRITING CODE","Death, PROGRAMMING HASKELL And Taxes",81,1535703399)
   , (154,"Where Can You Find Free WRITING CODE Resources","10 Unforgivable Sins Of PROGRAMMING HASKELL",26,1523191411)
   , (155,"Can You Really Find WRITING CODE (on the Web)?","It's All About (The) PROGRAMMING HASKELL",73,1537705881)
   , (157,"WRITING CODE An Incredibly Easy Method That Works For All","5 Best Ways To Sell PROGRAMMING HASKELL",68,1540498888)
   , (158,"5 Sexy Ways To Improve Your WRITING CODE","Little Known Ways to PROGRAMMING HASKELL",24,1527691660)
   , (159,"Believe In Your WRITING CODE Skills But Never Stop Improving","2 Ways You Can Use PROGRAMMING HASKELL To Become Irresistible To Customers",10,1524060512)
   , (162,"Secrets To Getting WRITING CODE To Complete Tasks Quickly And Efficiently","Stop Wasting Time And Start PROGRAMMING HASKELL",97,1539718611)
   , (163,"The Quickest & Easiest Way To WRITING CODE","5 Ways Of PROGRAMMING HASKELL That Can Drive You Bankrupt - Fast!",2,1535840862)
   , (164,"The Death Of WRITING CODE And How To Avoid It","Marriage And PROGRAMMING HASKELL Have More In Common Than You Think",91,1540543227)
   , (170,"How To Learn WRITING CODE","You Can Thank Us Later - 3 Reasons To Stop Thinking About PROGRAMMING HASKELL",90,1516303439)
   , (178,"Learn Exactly How We Made WRITING CODE Last Month","A Guide To PROGRAMMING HASKELL At Any Age",41,1540880777)
   , (179,"WRITING CODE Iphone Apps","Sexy PROGRAMMING HASKELL",6,1536408945)
   , (184,"How To Turn WRITING CODE Into Success","Now You Can Have Your PROGRAMMING HASKELL Done Safely",46,1528016544)
   , (185,"5 Problems Everyone Has With WRITING CODE √ How To Solved Them","Apply These 5 Secret Techniques To Improve PROGRAMMING HASKELL",19,1538654893)
   , (186,"Apply These 5 Secret Techniques To Improve WRITING CODE","The Secret of PROGRAMMING HASKELL",58,1523596985)
   ]

getYoungUsersTest :: TestTree
getYoungUsersTest = testCase "Get Young Users Test" $ do
  youngUsers <- rq getYoungUsers
  youngUsers @?= expectedYoungUsers

expectedYoungUsers :: [Entity User]
expectedYoungUsers = mkUser <$>
   [ (35,"Tiffanie Delgado","Tiffanie@Delgado.com",19)
   , (46,"Stephnie Whitacre","Stephnie.Whitacre@gmail.com",20)
   , (34,"Renato Kimpel","Renato.Kimpel@test.com",20)
   , (23,"Philip Laverriere","Philip@Laverriere.com",21)
   , (67,"Micha Knop","Micha@Knop.com",21)
   , (63,"Mercedez Gonsales","Mercedez.Gonsales@gmail.com",22)
   , (42,"Leigha Risch","Leigha@Risch.com",22)
   , (79,"Lavada Fife","Lavada.Fife@test.com",19)
   , (11,"Kathline Grange","Kathline.Grange@gmail.com",19)
   , (31,"Kandy Dragoo","Kandy@Dragoo.com",18)
   ]

getSpecialPairsTest :: TestTree
getSpecialPairsTest = testCase "Get Special Pairs Tests" $ do
  specialPairs <- rq getSpecialPairs
  specialPairs @?= expectedSpecialPairs

expectedSpecialPairs :: [(Entity User, Entity Article)]
expectedSpecialPairs = mkPair <$>
   [ (95,"Etsuko Adamek","Etsuko.Adamek@test.com",53,108,"Essential PROGRAMMING HASKELL Smartphone Apps","WRITING CODE Works Only Under These Conditions",1544438329)
   , (7, "Tierra Servin","Tierra.Servin@test.com",49,180,"Top 3 Ways To Buy A Used PROGRAMMING HASKELL","WRITING CODE Smackdown!",1500274530)
   , (59,"Tim Hisle","Tim.Hisle@gmail.com",23,182,"Top 10 Tips With PROGRAMMING HASKELL","What Alberto Savoia Can Teach You About WRITING CODE",1548606279)
   ]
   where
     mkPair :: (Int64, Text, Text, Int, Int64, Text, Text, Integer) -> (Entity User, Entity Article)
     mkPair (uid, name, email, age, aid, body, title, ts) =
       (mkUser (uid, name, email, age), mkArticle (aid, title, body, uid, ts))

commentsFromUserTests :: TestTree
commentsFromUserTests = testCase "Get Comments From User Tests" $ do
  comments1 <- rq (getCommentsFromUser (toSqlKey 2))
  comments2 <- rq (getCommentsFromUser (toSqlKey 5))
  comments3 <- rq (getCommentsFromUser (toSqlKey 37))
  comments4 <- rq (getCommentsFromUser (toSqlKey 63))
  comments5 <- rq (getCommentsFromUser (toSqlKey 98))
  comments1 @?= trueComments1
  comments2 @?= trueComments2
  comments3 @?= trueComments3
  comments4 @?= trueComments4
  comments5 @?= trueComments5

commentsOnUserTests :: TestTree
commentsOnUserTests = testCase "Get Comments On User Tests" $ do
  comments1 <- rq (getCommentsOnUser (toSqlKey 4))
  comments2 <- rq (getCommentsOnUser (toSqlKey 8))
  comments3 <- rq (getCommentsOnUser (toSqlKey 16))
  comments4 <- rq (getCommentsOnUser (toSqlKey 32))
  comments5 <- rq (getCommentsOnUser (toSqlKey 66))
  comments1 @?= trueComments6
  comments2 @?= trueComments7
  comments3 @?= trueComments8
  comments4 @?= trueComments9
  comments5 @?= trueComments10
