import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (Decoder, map2, field, string)

main = Browser.element { init = init
                       , update = update
                       , subscriptions = \_ -> Sub.none
                       , view = view
                       }

type alias Product = { name : String, description : String }

productDecoder : Decoder Product
productDecoder = map2 Product (field "name" string) (field "description" string)

type alias Model = { productId : String, productStatus : ProductStatus }
type ProductStatus = JustStarted
                   | LoadingProduct
                   | Error
                   | ProductData Product

init : () -> (Model, Cmd Msg)
init _ = ( { productId = "", productStatus = JustStarted } , Cmd.none )

type Msg = TextboxChanged String | Load | ReceivedInfo (Result Http.Error Product)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  TextboxChanged pid -> ({ model | productId = pid }, Cmd.none)
  Load -> ( { model | productStatus = LoadingProduct }
          , Http.get
             { url = "http://practical.haskell/product/" ++ model.productId
             , expect = Http.expectJson ReceivedInfo productDecoder
             } )
  ReceivedInfo result -> case result of
    Ok p  -> ({ model | productStatus = ProductData p }, Cmd.none)
    Err _ -> ({ model | productStatus = Error }, Cmd.none)

view : Model -> Html Msg
view model = h1 [] []  -- Exercise to the reader