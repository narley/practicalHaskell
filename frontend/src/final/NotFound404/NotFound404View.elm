module NotFound404View exposing (..)

import Html exposing (..)

import Model exposing (..)
import NotFound404Model exposing (..)

notFound404View : NotFound404Model -> Html NotFound404Message
notFound404View _ = div [] [text "Not Found 404"]

notFound404Update : NotFound404Message -> NotFound404Model -> (NotFound404Model, Cmd NotFound404Message)
notFound404Update _ m = (m, Cmd.none)
