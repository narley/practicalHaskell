module Multipage exposing (..)

import Browser
import Browser.Navigation exposing (Key, pushUrl, load)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Json.Decode
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

type Route = Landing | Article Int | Login | NotFound404

routeParser : Parser (Route -> a) a
routeParser = oneOf []

parseUrl : Url -> Route
parseUrl _ = NotFound404

main : Program () Model Message
main = Browser.application
  { init = (\_ initialUrl initialKey -> (Model initialKey (parseUrl initialUrl), Cmd.none))
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  , onUrlRequest = \_ -> NoMessage
  , onUrlChange = \_ -> NoMessage
  }

view : Model -> Browser.Document Message
view _ =
  let body = view404
  in {title = "Article Program", body = [body]}

view404 : Html Message
view404 = div [] [text "Sorry, this page doesn't exit!"]

update : Message -> Model -> (Model, Cmd Message)
update msg model = case msg of
  NoMessage -> (model, Cmd.none)
