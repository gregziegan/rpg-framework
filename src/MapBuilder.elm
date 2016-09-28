module MapBuilder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import GameBoard exposing (Tile, initTile)
import Helpers


type alias Model =
    { tilePreview : Tile
    , tileName : String
    , tileImage : String
    }


init : ( Model, Cmd Msg )
init =
    { tilePreview = initTile "" ""
    , tileName = ""
    , tileImage = ""
    }
        ! []


type Msg
    = SetTileName String
    | SetTileImage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTileName name ->
            let
                tilePreview =
                    model.tilePreview

                newTile =
                    { tilePreview | name = name }
            in
                { model
                    | tileName = name
                    , tilePreview = newTile
                }
                    ! []

        SetTileImage image ->
            let
                tilePreview =
                    model.tilePreview

                newTile =
                    { tilePreview | image = image }
            in
                { model
                    | tileImage = image
                    , tilePreview = newTile
                }
                    ! []


viewTileBuilder : Model -> Html Msg
viewTileBuilder model =
    div []
        [ label [] [ text "Tile Name" ]
        , input [ onInput SetTileName, value model.tileName ] []
        , label [] [ text "Tile Image" ]
        , input [ onInput SetTileImage, value model.tileImage ] []
        ]


viewTile : Tile -> Html Msg
viewTile tile =
    div
        [ class "gameTile"
        , style
            [ Helpers.setBackgroundAsSprite tile.image
            ]
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ viewTile model.tilePreview
        , viewTileBuilder model
        ]
