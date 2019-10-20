module NewArticleView exposing (..)

import Html exposing (..)

import Model exposing (..)
import NewArticleModel exposing (..)

newArticleView : NewArticleModel -> Html NewArticleMessage
newArticleView _ = div [] [text "New Article"]

newArticleUpdate : NewArticleMessage -> NewArticleModel -> (NewArticleModel, Cmd NewArticleMessage)
newArticleUpdate _ m = (m, Cmd.none)
