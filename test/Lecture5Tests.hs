{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int (Int64)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Sql (toSqlKey, entityKey, Entity(..))
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Lecture5
import Schema

main :: IO ()
main = defaultMain $ testGroup "Lecture 5 Tests"
  [fetch100Test, lastYearsArticlesTest, getYoungUsersTest]

fetch100Test :: TestTree
fetch100Test = testCase "Fetch 100 Test" $ do
  info <- fetch100
  fst info @?= "Apply These 5 Secret Techniques To Improve PROGRAMMING HASKELL"
  snd info @?= posixSecondsToUTCTime 1539272498

lastYearsArticlesTest :: TestTree
lastYearsArticlesTest = testCase "Last Year's Articles Test" $ do
  articles <- lastYearsArticles
  let trueArticles = sortBy sortByTitle expectedArticles
  length articles @?= length trueArticles
  mapM_ (\(a1, a2) -> entityKey a1 @?= entityKey a2) (zip articles trueArticles)

getYoungUsersTest :: TestTree
getYoungUsersTest = testCase "Get Young Users Test" $ do
  users <- getYoungUsers
  users @?= expectedUsers

mkArticleEntity :: (Int64, Text, Text, Integer) -> Entity Article
mkArticleEntity (id_, title, body, time) =
  Entity (toSqlKey id_) (Article title body (posixSecondsToUTCTime (fromInteger time)))

sortByTitle :: Entity Article -> Entity Article -> Ordering
sortByTitle (Entity _ a1) (Entity _ a2) = compare (articleTitle a1) (articleTitle a2)

expectedArticles :: [Entity Article]
expectedArticles = mkArticleEntity <$>
  [ (2,"You Don't Have To Be A Big Corporation To Start PROGRAMMING HASKELL","WRITING CODE Is Essential For Your Success. Read This To Find Out Why",1536627837)
  , (7,"What You Should Have Asked Your Teachers About PROGRAMMING HASKELL","Don't Be Fooled By WRITING CODE",1519475985)
  , (9,"3 Ways To Have (A) More Appealing PROGRAMMING HASKELL","Essential WRITING CODE Smartphone Apps",1533367094)
  , (10,"4 Ways You Can Grow Your Creativity Using PROGRAMMING HASKELL","How To Take The Headache Out Of WRITING CODE",1525666889)
  , (11,"What Everyone Must Know About PROGRAMMING HASKELL","Don't Just Sit There! Start WRITING CODE",1515148424)
  , (13,"PROGRAMMING HASKELL Your Way To Success","The Best Way To WRITING CODE",1529202282)
  , (16,"12 Questions Answered About PROGRAMMING HASKELL","No More Mistakes With WRITING CODE",1524820794)
  , (20,"How To Turn PROGRAMMING HASKELL Into Success","Now You Can Have The WRITING CODE Of Your Dreams √ Cheaper/Faster Than You Ever Imagined",1544086085)
  , (21,"5 Ways PROGRAMMING HASKELL Will Help You Get More Business","Here Is A Method That Is Helping WRITING CODE",1543950004)
  , (23,"Essential PROGRAMMING HASKELL Smartphone Apps","WRITING CODE Works Only Under These Conditions",1532555497)
  , (26,"PROGRAMMING HASKELL: This Is What Professionals Do","Why Everything You Know About WRITING CODE Is A Lie",1537110990)
  , (30,"Got Stuck? Try These Tips To Streamline Your PROGRAMMING HASKELL","Never Changing WRITING CODE Will Eventually Destroy You",1537094048)
  , (31,"PROGRAMMING HASKELL An Incredibly Easy Method That Works For All","Revolutionize Your WRITING CODE With These Easy-peasy Tips",1542317536)
  , (33,"10 Warning Signs Of Your PROGRAMMING HASKELL Demise","Where Is The Best WRITING CODE?",1515368319)
  , (35,"5 Simple Steps To An Effective PROGRAMMING HASKELL Strategy","The Philosophy Of WRITING CODE",1515928489)
  , (36,"22 Tips To Start Building A PROGRAMMING HASKELL You Always Wanted","How To Save Money with WRITING CODE?",1541020076)
  , (40,"Don't Be Fooled By PROGRAMMING HASKELL","Fall In Love With WRITING CODE",1528079918)
  , (42,"Double Your Profit With These 5 Tips on PROGRAMMING HASKELL","Is WRITING CODE Worth [$] To You?",1528975337)
  , (43,"Ho To  , (Do) PROGRAMMING HASKELL Without Leaving Your Office, (House).","The Secrets To WRITING CODE",1534652851)
  , (44,"Top 10 Tips To Grow Your PROGRAMMING HASKELL","3 Easy Ways To Make WRITING CODE Faster",1515506762)
  , (49,"Being A Star In Your Industry Is A Matter Of PROGRAMMING HASKELL","Who Else Wants To Know The Mystery Behind WRITING CODE?",1542864499)
  , (51,"PROGRAMMING HASKELL Works Only Under These Conditions","In 10 Minutes, I'll Give You The Truth About WRITING CODE",1523703492)
  , (53,"15 Tips For PROGRAMMING HASKELL Success","The Hidden Mystery Behind WRITING CODE",1529821436)
  , (54,"Can You Really Find PROGRAMMING HASKELL  , (on the Web)?","Fear? Not If You Use WRITING CODE The Right Way!",1538438897)
  , (56,"How To Win Friends And Influence People with PROGRAMMING HASKELL","How WRITING CODE Made Me A Better Salesperson",1516504234)
  , (57,"Who Else Wants To Know The Mystery Behind PROGRAMMING HASKELL?","Winning Tactics For WRITING CODE",1520253019)
  , (59,"11 Methods Of PROGRAMMING HASKELL Domination","5 Ways Of WRITING CODE That Can Drive You Bankrupt - Fast!",1525238692)
  , (61,"10 Tips That Will Make You Influential In PROGRAMMING HASKELL","The Number One Reason You Should (Do) WRITING CODE",1536774102)
  , (64,"Fall In Love With PROGRAMMING HASKELL","WRITING CODE: An Incredibly Easy Method That Works For All",1531204199)
  , (65,"Death, PROGRAMMING HASKELL And Taxes","Here Is A Quick Cure For WRITING CODE",1545380834)
  , (66,"Why I Hate PROGRAMMING HASKELL","3 WRITING CODE Secrets You Never Knew",1532069294)
  , (69,"10 Unforgivable Sins Of PROGRAMMING HASKELL","Where Can You Find Free WRITING CODE Resources",1526379727)
  , (72,"5 Best Ways To Sell PROGRAMMING HASKELL","WRITING CODE An Incredibly Easy Method That Works For All",1521344862)
  , (80,"PROGRAMMING HASKELL And Love - How They Are The Same","Take 10 Minutes to Get Started With WRITING CODE",1531712500)
  , (83,"Remarkable Website - PROGRAMMING HASKELL Will Help You Get There","What Is WRITING CODE and How Does It Work?",1540369645)
  , (84,"Is PROGRAMMING HASKELL Worth [$] To You?","How To Lose Money With WRITING CODE",1537518526)
  , (86,"How To Use PROGRAMMING HASKELL To Desire","How To Start A Business With WRITING CODE",1515383201)
  , (89,"Wondering How To Make Your PROGRAMMING HASKELL Rock? Read This!","3 Things Everyone Knows About WRITING CODE That You Don't",1545957679)
  , (94,"Sexy PROGRAMMING HASKELL","WRITING CODE Iphone Apps",1515890610)
  , (99,"Now You Can Have Your PROGRAMMING HASKELL Done Safely","How To Turn WRITING CODE Into Success",1524704303)
  , (100,"Apply These 5 Secret Techniques To Improve PROGRAMMING HASKELL","5 Problems Everyone Has With WRITING CODE √ How To Solved Them",1539272498)
  ]

mkUserEntity :: Int64 -> Text -> Text -> Int -> Entity User
mkUserEntity id_ name email age = Entity (toSqlKey id_) (User name email age)

expectedUsers :: [Entity User]
expectedUsers =
  [ mkUserEntity 35 "Tiffanie Delgado" "Tiffanie@Delgado.com" 19
  , mkUserEntity 46 "Stephnie Whitacre" "Stephnie.Whitacre@gmail.com" 20
  , mkUserEntity 34 "Renato Kimpel" "Renato.Kimpel@test.com" 20
  , mkUserEntity 23 "Philip Laverriere" "Philip@Laverriere.com" 21
  , mkUserEntity 67 "Micha Knop" "Micha@Knop.com" 21
  , mkUserEntity 63 "Mercedez Gonsales" "Mercedez.Gonsales@gmail.com" 22
  , mkUserEntity 42 "Leigha Risch" "Leigha@Risch.com" 22
  , mkUserEntity 79 "Lavada Fife" "Lavada.Fife@test.com" 19
  , mkUserEntity 11 "Kathline Grange" "Kathline.Grange@gmail.com" 19
  , mkUserEntity 31 "Kandy Dragoo" "Kandy@Dragoo.com" 18
  ]
