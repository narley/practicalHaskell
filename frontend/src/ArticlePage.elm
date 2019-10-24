module ArticlePage exposing (..)

import Browser
import Html exposing (text, div, Html, button, br, input, h1, p, h2, a)
import Html.Attributes exposing (value, href)
import Html.Events exposing (onClick, onInput)

import Http exposing (..)
import Url exposing (..)
import Url.Builder exposing (..)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, top, parse, string)

import SchemaTypes exposing (..)

type alias Model =
  { currentArticle: Maybe Article
  , currentInput: String
  }

type Message =
  NoMessage |
  ChangedInput String |
  LoadedArticle Article |
  CallFailed |
  PressedRefresh

main : Program () Model Message
main = Browser.application
  { init = (\_ _ _ -> (Model Nothing "", Cmd.none))
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  , onUrlRequest = \_ -> NoMessage
  , onUrlChange = \_ -> NoMessage
  }

view : Model -> Browser.Document Message
view {currentArticle, currentInput} =
  let articleSection = case currentArticle of
        Nothing -> div [] [text "No Article!"]
        Just a -> div []
          [ h1 [] [text a.articleTitle]
          , p [] [text a.articleBody]
          ]
      body = div []
        [ input [value currentInput, onInput ChangedInput] []
        , button [onClick PressedRefresh] [text "Refresh"]
        , br [] []
        , articleSection
        ]
  in {title = "Article Program", body = [body]}

update : Message -> Model -> (Model, Cmd Message)
update msg ({currentArticle, currentInput} as m) = case msg of
  NoMessage -> (m, Cmd.none)
  CallFailed -> ({m | currentArticle = Nothing}, Cmd.none)
  LoadedArticle article -> ({m | currentArticle = Just article}, Cmd.none)
  ChangedInput newInput -> ({m | currentInput = newInput}, Cmd.none)
  PressedRefresh -> (m, sendArticleRequest currentInput)

sendArticleRequest : String -> Cmd Message
sendArticleRequest aid = case String.toInt aid of
  Nothing -> Cmd.none
  Just i -> Http.get
      { url = Url.toString (localUrl i)
      , expect = Http.expectJson processArticleResponse jsonDecArticle
      }

processArticleResponse : Result Error Article -> Message
processArticleResponse result = case result of
  Err _ -> CallFailed
  Ok article -> LoadedArticle article

localUrl : Int -> Url
localUrl articleId =
  { protocol = Http
  , host = "localhost"
  , port_ = Just 8080
  , path = absolute ["api", "articles", String.fromInt articleId] []
  , query = Nothing
  , fragment = Nothing
  }
