module LandingView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)

import LandingModel exposing (..)
import Model exposing (..)

landingView : LandingModel -> Html LandingMessage
landingView _ = div [] [text "Landing", br [] [], a [href "/articles/94"] [text "An Article"]]

landingUpdate : LandingMessage -> LandingModel -> (LandingModel, Cmd LandingMessage)
landingUpdate _ m = (m, Cmd.none)
