module BlogLandingBasic exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Random exposing (..)
import Set exposing (..)
import String exposing (..)
import Time exposing (..)
import Task exposing (..)

type alias Article =
    { articleId : Int
    , articleTitle : String
    , articleBody : String
    , articlePublishedAt : Posix
    }

type alias BlogLandingModel =
    { currentArticles : List Article
    , titleInput : String
    , bodyTextarea : String
    , title : String
    , body : String
    , removedIds : List Int
    , currentTime : Posix
    , zone : Time.Zone
    }

type BlogLandingMessage
    = ChangeInput String
    | ChangeTextArea String
    | Submit
    | Remove Int
    | GenerateTitle
    | TemplateNumberAdjective RandomTitle
    | ReceivedTimeUpdate Time.Posix
    | CurrentTime Time.Posix
    | Zone Time.Zone
    | NoMessage

main : Program () BlogLandingModel BlogLandingMessage
main =
    Browser.application
        { init = \_ _ _ -> ( BlogLandingModel initialArticles "" "" "" "" [] (millisToPosix 0) Time.utc, Task.perform Zone Time.here)
        , onUrlChange = \_ -> NoMessage
        , onUrlRequest = \_ -> NoMessage
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


view : BlogLandingModel -> Browser.Document BlogLandingMessage
view model =
    let body = div []
          (h1 [] [ text "Blog Previews" ]
              :: List.map articleBlock ((List.take 5 << List.reverse << getNotRemoved model.removedIds << List.sortBy .articleId) model.currentArticles)
              ++ [blogForm model]
          )
     in { title = "", body = [body]}

flipAppend : String -> String -> String
flipAppend xs ys =
    ys ++ xs

getNotRemoved : List Int -> List Article -> List Article
getNotRemoved removedIds =
    List.filter (\article -> not <| List.member article.articleId removedIds)

articleBlock : Article -> Html BlogLandingMessage
articleBlock { articleId, articleTitle, articleBody, articlePublishedAt } =
    div
        [ style "border-style" "solid"
        , style "margin" "10px"
        ]
        [ h2 [] [ text (String.fromInt articleId ++ " - " ++ articleTitle) ]
        , p [] [ (text << flipAppend "..." << left 50) articleBody ]
        , p
              [ style "font-weight" "bold" ]
              [ text ("Published: " ++ (posixToDateTime (Time.customZone 60 []) articlePublishedAt))]
        , button
              [ style "cursor" "pointer"
              , onClick <| Remove articleId
              ]
              [ text "X" ]
        ]

blogForm : BlogLandingModel -> Html BlogLandingMessage
blogForm {titleInput, bodyTextarea} =
    div []
        [ h3 [] [ text "Write an article!" ]
        , div [ style "margin-bottom" "10px" ]
            [ label [] [ text "Title:" ]
            , br [] []
            , input
                  [ onInput ChangeInput
                  , value titleInput
                  , type_ "text"
                  , style "width" "500px"
                  ] []
            , button
                  [ onClick GenerateTitle
                  , style "cursor" "pointer"
                  , style "margin-left" "10px"
                  ]
                  [ text "Generate Title" ]
            ]
        , div [ style "margin-bottom" "10px" ]
            [ label [] [ text "Content:" ]
            , br [] []
            , textarea
                  [ onInput ChangeTextArea
                  , value bodyTextarea
                  ] []
            ]
        , button
              [ onClick Submit
              , style "cursor" "pointer"
              , disabled <| List.any String.isEmpty [titleInput, bodyTextarea]
              ] [ text "Submit!" ]
        ]

update : BlogLandingMessage -> BlogLandingModel -> (BlogLandingModel, Cmd BlogLandingMessage)
update msg model =
    case msg of
        ChangeInput title -> ({ model | titleInput = title }, Cmd.none)
        ChangeTextArea body -> ({ model | bodyTextarea = body }, Cmd.none)
        Submit ->
            let
                lastArticleId =
                    case (List.head << List.reverse << List.sortBy .articleId) model.currentArticles of
                        Just { articleId } -> articleId
                        Nothing -> 0
                newArticle = Article (lastArticleId + 1) model.titleInput model.bodyTextarea model.currentTime
            in
                ({model
                    | currentArticles = newArticle :: model.currentArticles
                    , titleInput = ""
                    , bodyTextarea = ""
                }, Cmd.none)
        Remove articleId -> ({ model | removedIds = articleId :: model.removedIds }, Cmd.none)
        GenerateTitle -> (model, templateNumberAdjectiveGenerator)
        TemplateNumberAdjective {templateRN, numberRN, adjectiveRN}  ->
            let
                randomTemplate = Maybe.withDefault ( "", "", "" ) <| List.head <| List.drop templateRN titleTemplates
                randomNumber = Maybe.withDefault "" <| List.head <| List.drop numberRN titleNumbers
                randomAdjective = Maybe.withDefault "" <| List.head <| List.drop adjectiveRN titleAdjectives
                newTitle = makeTitle randomTemplate randomNumber randomAdjective
            in
                ({ model | titleInput = newTitle }, Cmd.none)
        ReceivedTimeUpdate time -> ({ model | currentTime = time }, Cmd.none)
        CurrentTime time -> ({ model | currentTime = time }, Cmd.none)
        Zone zone -> ({ model | zone = zone }, Task.perform CurrentTime Time.now)
        NoMessage -> (model, Cmd.none)

subscriptions : BlogLandingModel -> Sub BlogLandingMessage
subscriptions _ = Time.every 1000 ReceivedTimeUpdate

posixToDateTime : Time.Zone -> Time.Posix -> String
posixToDateTime zone posix =
    let
        year = String.fromInt <| toYear zone posix
        month =
            case toMonth zone posix of
                Jan -> "1"
                Feb -> "2"
                Mar -> "3"
                Apr -> "4"
                May -> "5"
                Jun -> "6"
                Jul -> "7"
                Aug -> "8"
                Sep -> "9"
                Oct -> "10"
                Nov -> "11"
                Dec -> "12"
        day = String.padLeft 2 '0' <| String.fromInt <| toDay zone posix
        hour = String.padLeft 2 '0' <| String.fromInt <| toHour zone posix
        minute = String.padLeft 2 '0' <| String.fromInt <| toMinute zone posix
    in
        day ++ "/" ++ month ++ "/" ++ year ++ " @ " ++ hour ++ ":" ++ minute

initialArticles : List Article
initialArticles =
    [ { articleId = 1
      , articleTitle = "The Number One Reason You Should Program in Haskell"
      , articleBody = "There are a lot of different programming languages out there. But most of them aren't as good as Haskell for a lot of different reasons."
      , articlePublishedAt = millisToPosix 1570318123000
      }
    , { articleId = 2
      , articleTitle = "Are You Making These 5 Haskell Mistakes?"
      , articleBody = "Haskell is a great language, but there are several ways you can go wrong. For instance, you don't want to try to take on a big project without knowing monads."
      , articlePublishedAt = millisToPosix 1570398123000
      }
    , { articleId = 3
      , articleTitle = "What Everyone Ought to Know about Programming in Haskell"
      , articleBody = "It takes quite a bit of time to write good, solid code in Haskell. With the right steps, you can quickly become proficient. But you can't just jump right in."
      , articlePublishedAt = millisToPosix 1570478123000
      }
    , { articleId = 4
      , articleTitle = "How to Become Better with Haskell in 10 Minutes"
      , articleBody = "Once you have a good grasp of the language mechanics, it will help you a lot to learn the Stack tool. This will make it very easy to bring in extra library code you need."
      , articlePublishedAt = millisToPosix 1570558123000
      }
    , { articleId = 5
      , articleTitle = "Programming Your Way to Success with Haskell"
      , articleBody = "Haskell is a great language because it's static type system makes it easier to avoid a big category of bugs. You'll ship products that fail much less often and require less testing."
      , articlePublishedAt = millisToPosix 1570638123000
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

type alias RandomTitle =
    { templateRN : Int
    , numberRN : Int
    , adjectiveRN : Int
    }

zeroToNine : Random.Generator Int
zeroToNine = Random.int 0 9

templateNumberAdjective : Random.Generator RandomTitle
templateNumberAdjective = Random.map3 RandomTitle zeroToNine zeroToNine zeroToNine

templateNumberAdjectiveGenerator : Cmd BlogLandingMessage
templateNumberAdjectiveGenerator = Random.generate TemplateNumberAdjective templateNumberAdjective
