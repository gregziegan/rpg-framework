module MapBuilder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Map exposing (Tile, initTile)
import Helpers
import Matrix exposing (Matrix)
import Json.Decode as Json


type alias Model =
    { tilePreview : Tile
    , tileName : String
    , tileImage : String
    , createdTiles : List Tile
    , gameMapPreview : Matrix TileSelector
    }


type alias TileSelector =
    { tile : Tile
    , showOptions : Bool
    }


initTileSelector tile =
    { tile = tile
    , showOptions = False
    }


init : ( Model, Cmd Msg )
init =
    { tilePreview = initTile "" ""
    , tileName = ""
    , tileImage = ""
    , createdTiles = []
    , gameMapPreview =
        Map.initTile "grass" "./assets/PathAndObjects.png"
            |> initTileSelector
            |> Matrix.repeat 5 5
    }
        ! []


type Msg
    = SetTileName String
    | SetTileImage String
    | CreateTile
    | ToggleOptions ( Int, Int ) TileSelector
    | ChangeTile ( Int, Int ) TileSelector Tile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
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

        ToggleOptions ( x, y ) tileSelector ->
            let
                newTileSelector =
                    { tileSelector | showOptions = not tileSelector.showOptions }

                newGameMapPreview =
                    Matrix.set x y newTileSelector model.gameMapPreview
            in
                { model
                    | gameMapPreview = newGameMapPreview
                }
                    ! []

        ChangeTile ( x, y ) tileSelector newTile ->
            let
                newTileSelector =
                    { tileSelector | tile = newTile }

                newGameMapPreview =
                    Matrix.set x y newTileSelector model.gameMapPreview
            in
                { model | gameMapPreview = newGameMapPreview } ! []


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
            |> Matrix.indexedMap (viewMapTile model)
            |> Map.matrixToDivs
        ]


viewMapTile : Model -> Int -> Int -> TileSelector -> Html Msg
viewMapTile model x y ({ tile, showOptions } as tileSelector) =
    div
        [ class "gameTile"
        , style
            [ Helpers.setBackgroundAsSprite tile.image
            ]
        , onClick <| ToggleOptions ( x, y ) tileSelector
        ]
        [ if showOptions then
            viewTileSelector model ( x, y ) tileSelector
          else
            div [] []
        ]


viewTileSelector : Model -> ( Int, Int ) -> TileSelector -> Html Msg
viewTileSelector model pos tileSelector =
    let
        stopProp =
            { defaultOptions | stopPropagation = True }

        viewTileOption tile =
            li
                [ class "tile-option"
                ]
                [ button
                    [ onWithOptions "click" stopProp (Json.succeed <| ChangeTile pos tileSelector tile)
                    ]
                    [ text tile.name ]
                ]
    in
        ul [ class "tile-selector" ]
            (List.map viewTileOption model.createdTiles)
