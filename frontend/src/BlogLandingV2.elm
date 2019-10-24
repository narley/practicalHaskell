module BlogLandingFinal exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (list)
import Url exposing (..)
import Url.Builder exposing (..)

import SchemaTypes exposing (..)

type alias BlogLandingModel =
  { currentArticles: List (Entity Article)
  }

type BlogLandingMessage = NoMessage

main : Program () BlogLandingModel BlogLandingMessage
main = Browser.application
  { init = (\_ _ _ -> (BlogLandingModel [], Cmd.none))
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  , onUrlRequest = \_ -> NoMessage
  , onUrlChange = \_ -> NoMessage
  }

view : BlogLandingModel -> Browser.Document BlogLandingMessage
view articles =
  let body = div [] []
  in  {title = "Blog", body = [body]}

update : BlogLandingMessage -> BlogLandingModel -> (BlogLandingModel, Cmd BlogLandingMessage)
update _ m = (m, Cmd.none)
