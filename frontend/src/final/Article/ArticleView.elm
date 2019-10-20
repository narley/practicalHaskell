module ArticleView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)

import ArticleModel exposing (..)
import Model exposing (..)

articleView : ArticleModel -> Html ArticleMessage
articleView _ = div [] [text "Article View", br [] [], a [href "/"] [text "Landing"]]

articleUpdate : ArticleMessage -> ArticleModel -> (ArticleModel, Cmd ArticleMessage)
articleUpdate _ m = (m, Cmd.none)
