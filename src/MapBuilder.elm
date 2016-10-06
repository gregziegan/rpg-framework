module MapBuilder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Map exposing (Tile, initTile)
import Helpers
import Matrix exposing (Matrix)


type alias Model =
    { tilePreview : Tile
    , tileName : String
    , tileImage : String
    , createdTiles : List Tile
    , gameMapPreview : Matrix Tile
    }


init : ( Model, Cmd Msg )
init =
    { tilePreview = initTile "" ""
    , tileName = ""
    , tileImage = ""
    , createdTiles = []
    , gameMapPreview = Matrix.repeat 5 5 (Map.initTile "grass" "./assets/PathAndObjects.png")
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
    div [ class "tile-builder" ]
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
    div [ class "created-tiles" ]
        (List.map viewTile tiles)


view : Model -> Html Msg
view model =
    div [ class "map-builder" ]
        [ viewCreatedTiles model.createdTiles
        , viewTileBuilder model
        , viewMapPreview model
        ]


viewMapPreview : Model -> Html Msg
viewMapPreview model =
    div [ class "map-preview" ]
        [ model.gameMapPreview
            |> Matrix.map viewTile
            |> Map.matrixToDivs
        ]


viewMapTile : Model -> Int -> Int -> Tile -> Html Msg
viewMapTile model x y tile =
    div
        [ class "gameTile"
        , style
            [ Helpers.setBackgroundAsSprite tile.image
            ]
        ]
        []
