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
    , createdTiles : List Tile
    }


init : ( Model, Cmd Msg )
init =
    { tilePreview = initTile "" ""
    , tileName = ""
    , tileImage = ""
    , createdTiles = []
    }
        ! []


type Msg
    = SetTileName String
    | SetTileImage String
    | CreateTile


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

        CreateTile ->
            { model
                | createdTiles = model.tilePreview :: model.createdTiles
                , tileName = ""
                , tileImage = ""
                , tilePreview = initTile "" ""
            }
                ! []


viewTileBuilder : Model -> Html Msg
viewTileBuilder model =
    div []
        [ viewTile model.tilePreview
        , label [] [ text "Tile Name" ]
        , input [ onInput SetTileName, value model.tileName ] []
        , label [] [ text "Tile Image" ]
        , input [ onInput SetTileImage, value model.tileImage ] []
        , button [ onClick CreateTile ] [ text "Create Tile" ]
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


viewCreatedTiles : List Tile -> Html Msg
viewCreatedTiles tiles =
    div []
        (List.map viewTile tiles)


view : Model -> Html Msg
view model =
    div []
        [ viewCreatedTiles model.createdTiles
        , viewTileBuilder model
        ]
