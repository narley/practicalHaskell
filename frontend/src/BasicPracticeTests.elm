module BasicPracticeTests exposing (..)

import Browser
import Html exposing (Html, div, h1, ul, li, text)
import Html.Attributes exposing (style)

import BasicPractice exposing (..)

type alias BPTestModel =
  { calculateAreaTests: List (String, Float, Float)
  , sumsAndProductsTests: List (String, List Int, List Int)
  }
type BPTestMessage = BPTestMessage

main = Browser.sandbox {init = loadModel, update = update, view = view}

loadModel : BPTestModel
loadModel =
  { calculateAreaTests =
    [ ("Circle 5 (0,0) should be 78.5398", 78.5398, calculateArea (Circle 5.0 {x: 0.0, y: 0.0})
    , ("Circle 3 (3, 5) should be 28.2743", 28.27433, calculateArea (Circle 5.0 {x: 3.0, y: 5.0})
    , ("Triangle (0,0) (2,0), (0, 2) should be 2", 2.0, calculateArea (Triangle {x: 0.0, y: 0.0} {x: 2.0, y: 0.0} {x: 0.0, y: 2.0})
    , ("Triangle (5,5) (12,3), (8, 15) should be 38", 38.0, calculateArea (Triangle {x: 5.0, y: 5.0} {x: 12.0, y: 3.0} {x: 8.0, y: 15.0})
    ]
  , sumsAndProductsTests =
    [ ("[] 3 5 should be []", [], sumsAndProducts [] 3 5)
    , ("[1,2,3] 3 10 should be [6, 7, 5, 4, 4, 1]", [], sumsAndProducts [1, 2, 3] 3 10)
    , ("[7, 3, 2, 9, 8] 2 9 should be [0, -5, 4, 3, 5, 5, -2, -9, -1, -7]", [0, -5, 4, 3, 5, 5, -2, -9, -1, -7], sumsAndProducts [7, 3, 2, 9, 8] 2 9)
    ]
  }

update : BPTestMessage -> BPTestModel -> BPTestModel
update _ m = m

view : BPTestModel -> Html BPTestMessage
view m = div []
  [ h1 [] [text "Calculate Area Cases"]
  , ul [] (List.map areaCase m.calculateAreaTests)
  , h1 [] [text "Sum And Products Cases"]
  , ul [] (List.map spCase m.sumsAndProductsTests)
  ]

areaCase : (String, Float, Float) -> Html BPTestMessage
areaCase (description, expected, actual) =
  let isCorrect = expected - actual < 0.00001
      liStyle = if isCorrect then [style "text" "green"] else [style "text" "red"]
  in  li [liStyle] [text description, text ": ", text (String.fromFloat actual)]

spCase : (String, List Int, List Int) -> Html BPTestMessage
spCase (description, expected, actual) =
  let isCorrect = expected == actual
      liStyle = if isCorrect then [style "text" "green"] else [style "text" "red"]
      listString = "[" ++ String.join ", " (List.map String.fromInt actual) ++ "]"
  in  li [liStyle] [text description, text ": ", text listString]
