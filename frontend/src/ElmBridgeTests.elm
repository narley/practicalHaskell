module ElmBridgeTests exposing (..)

import Browser
import Html exposing (Html, text, div, p)
import Html.Attributes exposing (style)
import Time exposing (millisToPosix)

import SchemaTypes exposing (..)

main : Program () () ()
main = Browser.sandbox {init = (), view = view, update = update}

update : () -> () -> ()
update _ _ = ()

view : () -> Html ()
view _ = div []
  [p [style "color" "green"] [text "If this module compiles, you're done with Lecture 6!"]]

userEntity : Entity User
userEntity =
  { entityKey = 1
  , entityValue = User "James" "james@test.com" 25
  }

articleEntity : Entity Article
articleEntity =
  { entityKey = 5
  , entityValue = Article "A Title" "The Body" (millisToPosix 0) 1
  }

commentEntity : Entity Comment
commentEntity =
  { entityKey = 2
  , entityValue = Comment 1 5 "Great Article!" (millisToPosix 0)
  }

reaction : ArticleReaction
reaction = ArticleReaction 5 Nothing Love (Metadata (millisToPosix 0) 0 0 0)

loginInfo : LoginInfo
loginInfo = LoginInfo "username" "password"

loginResponse : LoginResponse
loginResponse = LoginResponse 5 "abcd"
