module Model exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)

import Url exposing (..)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, top, parse, string)

import ArticleModel exposing (..)
import LandingModel exposing (..)
import LoginModel exposing (..)

type Route =
  Landing |
  ArticlePage Int |
  LoginPage |
  NotFound404

routeParser : Parser (Route -> a) a
routeParser = oneOf
  [ map Landing top
  , map ArticlePage (s "articles" </> int)
  , map LoginPage (s "login")
  ]

parseUrl : Url -> Route
parseUrl url = Maybe.withDefault NotFound404 (parse routeParser url)

type alias AppModel =
  { navigationKey: Key
  , currentPage: Route
  , landingModel: LandingModel
  , articleModel: ArticleModel
  , loginModel: LoginModel
  }

type AppMessage =
  RequestedUrl UrlRequest |
  ChangedUrl Url |
  LandingMsg LandingMessage |
  ArticleMsg ArticleMessage |
  LoginMsg LoginMessage

initModel : Key -> Url -> AppModel
initModel k u =
  { navigationKey = k
  , currentPage = parseUrl u
  , landingModel = initLandingModel
  , articleModel = initArticleModel
  , loginModel = initLoginModel
  }
