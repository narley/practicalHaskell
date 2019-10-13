module JsonTests exposing (..)

import Browser
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Html exposing (Html, div, h1, ul, li, text)
import Html.Attributes exposing (style)

import PosixJson exposing (..)
import SchemaTypes exposing (..)
import Time exposing (..)

type alias JsonTestModel =
  { encodeTimeTests: List (String, String, String)
  , decodeTimeTests: List (String, Posix, Result Error Posix)
  , encodeEntityTests: List (String, String, String)
  , decodeEntityTest1: (String, Entity User, Result Error (Entity User))
  , decodeEntityTest2: (String, Entity Article, Result Error (Entity Article))
  }

main = Browser.sandbox {init = loadModel, update = update, view = view}

loadModel : JsonTestModel
loadModel =
  { encodeTimeTests =
    [ ("Encode 12/01/19 - 15:30:30", "2019-12-01T15:30:30", encode 0 (jsonEncPosix (millisToPosix 1575214230000)))
    , ("Encode 12/31/19 - 12:00:00", "2019-12-31T12:00:00", encode 0 (jsonEncPosix (millisToPosix 1577793600000)))
    , ("Encode 01/01/20 - 12:00:00", "2020-01-01T12:00:00", encode 0 (jsonEncPosix (millisToPosix 1577880000000)))
    , ("Encode 03/02/20 - 12:00:00", "2020-03-02T12:00:00", encode 0 (jsonEncPosix (millisToPosix 1583150400000)))
    ]
  , decodeTimeTests =
    [ ("Decode 12/01/19 - 15:30:30", millisToPosix 1575214230000, decodeString jsonDecPosix "2019-12-01T15:30:30")
    , ("Decode 12/31/19 - 12:00:00", millisToPosix 1577793600000, decodeString jsonDecPosix "2019-12-31T12:00:00")
    , ("Decode 01/01/20 - 12:00:00", millisToPosix 1577880000000, decodeString jsonDecPosix "2020-01-01T12:00:00+03")
    , ("Decode 03/02/20 - 12:00:00", millisToPosix 1583150400000, decodeString jsonDecPosix "2020-03-02T12:00:00+03")
    ]
  , encodeEntityTests =
    [ ( "Encode 'ID 5 User james james@test.com 25 should be {\"id\":5,\"userName\":\"james\",\"userEmail\":\"james@test.com\",\"userAge\":25}"
      , "{\"id\":5,\"userName\":\"james\",\"userEmail\":\"james@test.com\",\"userAge\":25"
      , encode 0 (encodeEntity jsonEncUser (Entity 5 (User "james" "james@test.com" 25)))
      )
    , ( "Encode 'ID 3 Article Title Body 2020-01-01T12:00:00 5 should be {\"id\":3,\"articleTitle\":\"Title\",\"articleBody\":\"Body\",\"articlePublishedAt\":\"2020-01-01T12:00:00+03\",\"articleAuthorId\":5}"
      , "{\"id\":3, \"articleTitle\":\"Title\",\"articleBody\":\"Body\",\"articlePublishedAt\":\"2020-01-01T12:00:00+03\",\"articleAuthorId\": 5}"
      , encode 0 (encodeEntity jsonEncArticle (Entity 3 (Article "Title" "Body" (millisToPosix 1577880000000) 5)))
      )
    ]
  , decodeEntityTest1 =
    ( "Decode Entity User should be ID 5 User james james@test.com 25"
    , Entity 5 (User "james" "james@test.com" 25)
    , decodeString (decodeEntity jsonDecUser) "{\"id\":5,\"userName\":\"james\",\"userEmail\":\"james@test.com\",\"userAge\":25"
    )
  , decodeEntityTest2 =
    ( "Decode Entity Article should be ID 3 Article Title Body 2020-01-01T12:00:00 5"
    , Entity 3 (Article "Title" "Body" (millisToPosix 1577880000000) 5)
    , decodeString (decodeEntity jsonDecArticle) "{\"id\":3, \"articleTitle\":\"Title\",\"articleBody\":\"Body\",\"articlePublishedAt\":\"2020-01-01T12:00:00+03\",\"articleAuthorId\": 5}"
    )
  }

update : () -> JsonTestModel -> JsonTestModel
update _ m = m

view : JsonTestModel -> Html ()
view m = div []
  [ h1 [] [text "Encode Time"]
  , ul [] (List.map encodeTimeCase m.encodeTimeTests)
  , h1 [] [text "Decode Time"]
  , ul [] (List.map decodeTimeCase m.decodeTimeTests)
  , h1 [] [text "Encode Entity"]
  , ul [] (List.map encodeEntityCase m.encodeEntityTests)
  , h1 [] [text "Decode Entity"]
  , ul [] [decodeEntityCase m.decodeEntityTest1, decodeEntityCase m.decodeEntityTest2]
  ]

encodeTimeCase : (String, String, String) -> Html ()
encodeTimeCase (description, expected, actual) =
  let isCorrect = expected == actual
      liStyle = if isCorrect then [style "color" "green"] else [style "color" "red"]
  in  li liStyle [text description, text ": ", text actual]

decodeTimeCase : (String, Posix, Result Error Posix) -> Html ()
decodeTimeCase (description, expected, actual) =
  let (isCorrect, resultString) = case actual of
        Err _ -> (False, "Could not parse!")
        Ok t -> if expected == t then (True, "Success!") else (False, "Wrong time!")
      liStyle = if isCorrect then [style "color" "green"] else [style "color" "red"]
  in  li liStyle [text description, text ": ", text resultString]

encodeEntityCase : (String, String, String) -> Html ()
encodeEntityCase (description, expected, actual) =
  let isCorrect = expected == actual
      liStyle = if isCorrect then [style "color" "green"] else [style "color" "red"]
  in  li liStyle [text description, text ": ", text actual]

decodeEntityCase : (String, a, Result Error a) -> Html ()
decodeEntityCase (description, expected, actual) =
  let (isCorrect, resultString) = case actual of
        Err _ -> (False, "Could not parse!")
        Ok t -> if expected == t then (True, "Success!") else (False, "Wrong time!")
      liStyle = if isCorrect then [style "color" "green"] else [style "color" "red"]
  in  li liStyle [text description, text ": ", text resultString]
