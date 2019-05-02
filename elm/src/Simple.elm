import Browser
import Html exposing (..)

main = Browser.sandbox { init = init, update = \_ model -> model, view = view }

type alias Model = { currentName : String }

init : Model
init = { currentName = "Alejandro" }

view : Model -> Html ()
view model
  = div [] [ text "Hello, ", text model.currentName, text "!"]

-- view2 : Model -> Html Msg
