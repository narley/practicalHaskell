module LoginView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)

import LoginModel exposing (..)

loginView : LoginModel -> Html LoginMessage
loginView _ = div [] [text "Login Page", br [] [], a [href "/"] [text "Landing"]]

loginUpdate: LoginMessage -> LoginModel -> (LoginModel, Cmd LoginMessage)
loginUpdate _ model = (model, Cmd.none)
