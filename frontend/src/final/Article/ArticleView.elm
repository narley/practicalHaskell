module ArticleView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Http exposing (..)
import Url exposing (..)
import Url.Builder exposing (..)

import ArticleModel exposing (..)
import Model exposing (hostString, portNum)
import SchemaTypes exposing (..)

articleView : ArticleModel -> Html ArticleMessage
articleView _ = div [] [text "Article View"]

articleUpdate : ArticleMessage -> ArticleModel -> (ArticleModel, Cmd ArticleMessage)
articleUpdate _ m = (m, Cmd.none)
