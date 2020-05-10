module BlogLanding exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Random exposing (..)
import Set exposing (..)
import String exposing (..)
import Time exposing (..)

type alias Article =
    { articleId : Int
    , articleTitle : String
    , articleBody : String
    }

type alias BlogLandingModel =
    { currentArticles : List Article
    , title : String
    , body : String
    , removedIds : List Int
    }

type BlogLandingMessage
    = ChangeInput String
    | ChangeTextArea String
    | Submit
    | Remove Int
    | NoMessage

main : Program () BlogLandingModel BlogLandingMessage
main =
    Browser.sandbox { init = BlogLandingModel initialArticles "" "" [], view = view, update = update }


view : BlogLandingModel -> Html BlogLandingMessage
view model =
    div []
        (h1 [] [ text "Blog Previews" ]
            :: List.map articleBlock ((List.take 5 << List.reverse << getNotRemoved model.removedIds << List.sortBy .articleId) model.currentArticles)
            ++ [blogForm model]
        )

flipAppend : String -> String -> String
flipAppend xs ys =
    ys ++ xs

getNotRemoved : List Int -> List Article -> List Article
getNotRemoved removedIds =
    List.filter (\article -> not <| List.member article.articleId removedIds)

articleBlock : Article -> Html BlogLandingMessage
articleBlock { articleId, articleTitle, articleBody } =
    div
        [ style "border-style" "solid"
        , style "margin" "10px"
        ]
        [ h2 [] [ text (String.fromInt articleId ++ " - " ++ articleTitle) ]
        , p [] [ (text << flipAppend "..." << left 50) articleBody ]
        , button
              [ style "cursor" "pointer"
              , onClick <| Remove articleId
              ]
              [ text "X" ]
        ]

blogForm : BlogLandingModel -> Html BlogLandingMessage
blogForm {title, body} =
    div []
        [ h3 [] [ text "Write an article!" ]
        , div [ style "margin-bottom" "10px" ]
            [ label [] [ text "Title:" ]
            , br [] []
            , input
                  [ onInput ChangeInput
                  , value title
                  , type_ "text"
                  ] []
            ]
        , div [ style "margin-bottom" "10px" ]
            [ label [] [ text "Content:" ]
            , br [] []
            , textarea
                  [ onInput ChangeTextArea
                  , value body
                  ] []
            ]
        , button
              [ onClick Submit
              , style "cursor" "pointer"
              , disabled <| List.any String.isEmpty [title, body]
              ] [ text "Submit!" ]
        ]

update : BlogLandingMessage -> BlogLandingModel -> BlogLandingModel
update msg model =
    case msg of
        ChangeInput title -> { model | title = title }
        ChangeTextArea body -> { model | body = body }
        Submit ->
            let
                lastArticleId =
                    case (List.head << List.reverse << List.sortBy .articleId) model.currentArticles of
                        Just { articleId } -> articleId
                        Nothing -> 0
                newArticle = Article (lastArticleId + 1) model.title model.body
            in
                {model
                    | currentArticles = newArticle :: model.currentArticles
                    , title = ""
                    , body = ""
                }
        Remove articleId -> { model | removedIds = articleId :: model.removedIds }
        NoMessage -> model


initialArticles : List Article
initialArticles =
    [ { articleId = 1
      , articleTitle = "The Number One Reason You Should Program in Haskell"
      , articleBody = "There are a lot of different programming languages out there. But most of them aren't as good as Haskell for a lot of different reasons."
      }
    , { articleId = 2
      , articleTitle = "Are You Making These 5 Haskell Mistakes?"
      , articleBody = "Haskell is a great language, but there are several ways you can go wrong. For instance, you don't want to try to take on a big project without knowing monads."
      }
    , { articleId = 3
      , articleTitle = "What Everyone Ought to Know about Programming in Haskell"
      , articleBody = "It takes quite a bit of time to write good, solid code in Haskell. With the right steps, you can quickly become proficient. But you can't just jump right in."
      }
    , { articleId = 4
      , articleTitle = "How to Become Better with Haskell in 10 Minutes"
      , articleBody = "Once you have a good grasp of the language mechanics, it will help you a lot to learn the Stack tool. This will make it very easy to bring in extra library code you need."
      }
    , { articleId = 5
      , articleTitle = "Programming Your Way to Success with Haskell"
      , articleBody = "Haskell is a great language because it's static type system makes it easier to avoid a big category of bugs. You'll ship products that fail much less often and require less testing."
      }
    ]


titleTemplates : List ( String, String, String )
titleTemplates =
    [ ( "", "", "Haskell Tricks No One Knows About" )
    , ( "Improve Your Haskell With These", "", "Techniques" )
    , ( "", "Ways to Make Testing Haskell More", "" )
    , ( "Learn Haskell in", "Steps.", "Results!" )
    , ( "Don't Make These", "", "Mistakes in Haskell!" )
    , ( "You'll Never Guess These", "Reasons Haskell is", "" )
    , ( "", "Uses for Haskell only", "Programmers Know About" )
    , ( "Never Write Haskell Again Without these", "", "Tools" )
    , ( "This Legendary Programmer Made", "Predictions About Haskell. You'll Never Believe the Last", "One!" )
    , ( "", "", "Uses for Haskell You've Never Thought About" )
    ]


titleAdjectives : List String
titleAdjectives =
    [ "Amazing"
    , "Horrible"
    , "Fantastic"
    , "Awesome"
    , "Terrible"
    , "New"
    , "Dumbfounding"
    , "Weird"
    , "Simple"
    , "Great"
    ]


titleNumbers : List String
titleNumbers =
    [ "Two"
    , "Three"
    , "Four"
    , "Five"
    , "Six"
    , "Seven"
    , "Eight"
    , "Nine"
    , "Ten"
    , "Eleven"
    ]


makeTitle : ( String, String, String ) -> String -> String -> String
makeTitle ( template1, template2, template3 ) number adjective =
    template1 ++ " " ++ number ++ " " ++ template2 ++ " " ++ adjective ++ " " ++ template3
