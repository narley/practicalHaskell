import Browser
import Html exposing (text, div, Html)

type Message = Message
type Model = Model

main : Program () Model Message
main = Browser.sandbox { init = Model, update = update, view = view }

update : Message -> Model -> Model
update message model = model

view : Model -> Html Message
view model = div [] [text "Hello World!"]
