import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

main = Browser.sandbox { init = init, update = update, view = view }

type alias Model = { currentName : String, textboxName : String }

init : Model
init = { currentName = "Alejandro", textboxName = "Alejandro" }

type Msg = TextboxChanged String
         | MakeCurrent

update : Msg -> Model -> Model
update msg model = case msg of
  TextboxChanged nm -> { model | textboxName = nm }
  MakeCurrent       -> { model | currentName = model.textboxName }

view : Model -> Html Msg
view model
  = div []
        [ div [] [ text "Hello, ", text model.currentName, text "!"]
        , input [ placeholder "Write your name here"
                , value model.textboxName
                , onInput TextboxChanged ] []
        , button [ onClick MakeCurrent ] [ text "Greet me!" ]
        ]

-- view2 : Model -> Html Msg
