module BlogLandingFinal exposing (..)

import Browser
import Html exposing (..)

import SchemaTypes exposing (..)

type alias BlogLandingModel = List Article
type BlogLandingMessage = NoMessage

main : Program () BlogLandingModel BlogLandingMessage
main = Browser.application
  { init = (\_ _ _ -> ([], Cmd.none))
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
