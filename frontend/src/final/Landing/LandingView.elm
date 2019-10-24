module LandingView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import Http exposing (..)
import Url exposing (..)
import Url.Builder exposing (..)

import LandingModel exposing (..)
import Model exposing (..)
import SchemaTypes exposing (..)

landingView : LandingModel -> Html LandingMessage
landingView _ = div [] [text "Landing"]

landingUpdate : LandingMessage -> LandingModel -> (LandingModel, Cmd LandingMessage)
landingUpdate _ m = (m, Cmd.none)
