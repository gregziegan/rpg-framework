module MapBuilder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)


type alias Model =
    { buildTile : String }


init : ( Model, Cmd Msg )
init =
    { buildTile = "Building..." } ! []


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


view : Model -> Html Msg
view model =
    div []
        [ text "hello world" ]
