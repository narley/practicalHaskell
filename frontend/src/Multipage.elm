module Multipage exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (text, div, Html, button, br, input, h1, p, h2)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)

import Http exposing (..)
import Url exposing (..)
import Url.Builder exposing (..)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, top, parse, string)

import SchemaTypes exposing (..)

type alias Model =
  { navigationKey: Key
  , currentPage: Route
  }

type Message = NoMessage

type Route = Landing | Article Int | LoginPage | NotFound404

routeParser : Parser (Route -> a) a
routeParser = oneOf []

parseUrl : Url -> Route
parseUrl _ = NotFound404

main : Program () Model Message
main = Browser.application
  { init = (\_ initialUrl initialKey -> (Model initialKey (parseUrl initialUrl), Cmd.none))
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = urlRequest
  , onUrlChange = changeUrl
  }

urlRequest : Browser.UrlRequest -> Message
urlRequest _ = NoMessage

changeUrl : Url -> Message
changeUrl _ = NoMessage

view : Model -> Browser.Document Message
view _ =
  let body = view404
  in {title = "Article Program", body = [body]}

view404 : Html Message
view404 = div [] [text "Sorry, this page doesn't exit!"]

update : Message -> Model -> (Model, Cmd Message)
update msg model = case msg of
  NoMessage -> (model, Cmd.none)

subscriptions : Model -> Sub Message
subscriptions _ = Sub.none
