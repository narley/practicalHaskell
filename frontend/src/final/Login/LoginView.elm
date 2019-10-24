module LoginView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Url exposing (..)
import Url.Builder exposing (..)

import LoginModel exposing (..)
import Model exposing (hostString, portNum)
import SchemaTypes exposing (..)

loginView : LoginModel -> Html LoginMessage
loginView _ = div [] [text "Login Page"]

loginUpdate: LoginMessage -> LoginModel -> (LoginModel, Cmd LoginMessage)
loginUpdate _ model = (model, Cmd.none)
