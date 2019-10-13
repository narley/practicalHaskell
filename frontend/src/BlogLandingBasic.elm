module BlogLanding exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)

type alias Article =
  { articleId: Int
  , articleTitle: String
  , articleBody: String
  }

type alias BlogLandingModel = List Article
type BlogLandingMessage = BlogLandingMessage

main : Program () BlogLandingModel BlogLandingMessage
main = Browser.sandbox {init = initialBlogModel, view = view, update = update}

view : BlogLandingModel -> Html BlogLandingMessage
view articles = div [] []

update : BlogLandingMessage -> BlogLandingModel -> BlogLandingModel
update _ m = m

initialBlogModel : List Article
initialBlogModel =
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

titleTemplates : List (String, String, String)
titleTemplates =
  [ ("", "", "Haskell Tricks No One Knows About")
  , ("Improve Your Haskell With These", "", "Techniques")
  , ("", "Ways to Make Testing Haskell More", "")
  , ("Learn Haskell in", "Steps.", "Results!")
  , ("Don't Make These", "", "Mistakes in Haskell!")
  , ("You'll Never Guess These", "Reasons Haskell is", "")
  , ("", "Uses for Haskell only", "Programmers Know About")
  , ("Never Write Haskell Again Without these", "", "Tools")
  , ("This Legendary Programmer Made", "Predictions About Haskell. You'll Never Believe the Last", "One!")
  , ("", "", "Uses for Haskell You've Never Thought About")
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

makeTitle : (String, String, String) -> String -> String -> String
makeTitle (template1, template2, template3) number adjective =
  template1 ++ " " ++ number ++ " " ++ template2 ++ " " ++ adjective ++ " " ++ template3