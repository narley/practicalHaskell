module LoginPage exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode exposing (list)
import Url exposing (..)
import Url.Builder exposing (..)

import SchemaTypes exposing (..)

type alias LoginPageModel =
  { currentLoginInfo : LoginInfo
  , lastLoginResult : Maybe Bool
  }
type LoginPageMessage =
  ChangedEmail String |
  ChangedPassword String |
  PressedSubmit |
  NoMessage

main : Program () LoginPageModel LoginPageMessage
main = Browser.application
  { init = (\_ _ _ -> (LoginPageModel (LoginInfo "" "") Nothing, Cmd.none))
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  , onUrlRequest = \_ -> NoMessage
  , onUrlChange = \_ -> NoMessage
  }

view : LoginPageModel -> Browser.Document LoginPageMessage
view {currentLoginInfo, lastLoginResult} =
  let body = div []
        [ input [onInput ChangedEmail, value currentLoginInfo.loginInfoUsername] []
        , br [] []
        , input [onInput ChangedPassword, value currentLoginInfo.loginInfoPassword] []
        , br [] []
        , button [onClick PressedSubmit] [text "Login"]
        , resultText lastLoginResult
        ]
  in  {title = "Blog", body = [body]}

update : LoginPageMessage -> LoginPageModel -> (LoginPageModel, Cmd LoginPageMessage)
update msg ({currentLoginInfo, lastLoginResult} as m) = case msg of
  ChangedEmail email -> (LoginPageModel (LoginInfo email currentLoginInfo.loginInfoPassword) lastLoginResult, Cmd.none)
  ChangedPassword password -> (LoginPageModel (LoginInfo currentLoginInfo.loginInfoUsername password) lastLoginResult, Cmd.none)
  PressedSubmit -> (m, Cmd.none)
  NoMessage -> (m, Cmd.none)

resultText : Maybe Bool -> Html LoginPageMessage
resultText llr = case llr of
  Nothing -> p [] [text "No login attempt yet"]
  Just True -> p [style "color" "green"] [text "Login attempt succeeded!"]
  Just False -> p [style "color" "red"] [text "Login attempt failed!"]

