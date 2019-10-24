import Browser
import Browser.Navigation exposing (pushUrl, load)
import Html exposing (..)
import Time exposing (..)
import Url

import ArticleView exposing (..)
import LandingView exposing (..)
import LoginView exposing (..)

import Model exposing (..)

main : Program () AppModel AppMessage
main = Browser.application
  { init = (\_ initialUrl initialKey -> (initModel initialKey initialUrl, pushUrl initialKey (Url.toString initialUrl)))
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = RequestedUrl
  , onUrlChange = ChangedUrl
  }

view : AppModel -> Browser.Document AppMessage
view model =
  let body = case model.currentPage of
        Landing -> Html.map LandingMsg (landingView model.landingModel)
        ArticlePage aid -> Html.map ArticleMsg (articleView model.articleModel)
        LoginPage -> Html.map LoginMsg (loginView model.loginModel)
        NotFound404 -> div [] [text "Not Found"]
  in {title = "Article Program", body = [body]}

update : AppMessage -> AppModel -> (AppModel, Cmd AppMessage)
update msg m = case msg of
  RequestedUrl urlRequest -> case urlRequest of
    Browser.External urlString -> (m, load urlString)
    Browser.Internal url -> (m, pushUrl m.navigationKey (Url.toString url))
  ChangedUrl u ->
    let nextPage = parseUrl u
        nextCommand = case nextPage of
          _ -> Cmd.none
    in  ({m | currentPage = nextPage}, nextCommand)
  LandingMsg landingMessage ->
    let (newLandingModel, cmd) = landingUpdate landingMessage (m.landingModel)
    in  ({m | landingModel = newLandingModel}, Cmd.map LandingMsg cmd)
  ArticleMsg articleMessage ->
    let (newArticleModel, cmd) = articleUpdate articleMessage (m.articleModel)
    in  ({m | articleModel = newArticleModel}, Cmd.map ArticleMsg cmd)
  LoginMsg loginMessage ->
    let (newLoginModel, cmd) = loginUpdate loginMessage (m.loginModel)
    in  ({m | loginModel = newLoginModel}, Cmd.map LoginMsg cmd)

subscriptions: AppModel -> Sub AppMessage
subscriptions _ = Sub.none
