module MapBuilder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Map exposing (Tile, initTile)
import Helpers
import Matrix exposing (Matrix)
import Json.Decode as Json
import Mouse exposing (Position)


type alias Model =
    { tilePreview : Tile
    , tileName : String
    , tileImage : String
    , createdTiles : List Tile
    , gameMapPreview : Matrix TileSelector
    , dragTile : Maybe DragTile
    , mousePosition : Position
    }


type alias DragTile =
    { start : Position
    , current : Position
    , tile : Tile
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
    , dragTile = Nothing
    , mousePosition = (Position 0 0)
    }
        ! []


type Msg
    = SetTileName String
    | SetTileImage String
    | CreateTile
    | ToggleOptions Position TileSelector
    | ChangeTile Position TileSelector Tile
    | TileDragStart Tile Position
    | TileDragAt Position
    | TileDragEnd Position


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

        ToggleOptions { x, y } tileSelector ->
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

        ChangeTile { x, y } tileSelector newTile ->
            let
                newTileSelector =
                    { tileSelector | tile = newTile }

                newGameMapPreview =
                    Matrix.set x y newTileSelector model.gameMapPreview
            in
                { model | gameMapPreview = newGameMapPreview } ! []

        TileDragStart tile xy ->
            { model | dragTile = Just { start = xy, current = xy, tile = tile }, mousePosition = xy } ! []

        TileDragAt xy ->
            let
                updateDragTilePos dragTile =
                    { dragTile | current = xy }

                updatedPosDragTile =
                    Maybe.map updateDragTilePos model.dragTile
            in
                { model | dragTile = model.dragTile, mousePosition = xy } ! []

        TileDragEnd position ->
            let
                newTileSelector tile =
                    { showOptions = False, tile = tile }

                newGameMapPreview tile =
                    case matrixPosForMousePos position of
                        Just ( x, y ) ->
                            Matrix.set x y (newTileSelector tile) model.gameMapPreview

                        Nothing ->
                            model.gameMapPreview
            in
                case model.dragTile of
                    Just { tile } ->
                        { model
                            | dragTile = Nothing
                            , mousePosition = position
                            , gameMapPreview = newGameMapPreview tile
                        }
                            ! []

                    Nothing ->
                        model ! []


matrixPosForMousePos : Position -> Maybe ( Int, Int )
matrixPosForMousePos pos =
    let
        x =
            toFloat pos.x

        y =
            (toFloat pos.y) - heightFromTopScreen

        heightFromTopScreen =
            120

        tileHeight =
            97

        tileWidth =
            97

        boardHeight =
            tileHeight * 5

        boardWidth =
            tileWidth * 5
    in
        if y < 120 || y > boardHeight then
            Nothing
        else if x > boardWidth then
            Nothing
        else
            Just <| ( floor (x / tileWidth), floor (y / tileHeight) )


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


viewCreatedTile : Model -> Tile -> Html Msg
viewCreatedTile model tile =
    div
        [ class "gameTile"
        , style
            [ Helpers.setBackgroundAsSprite tile.image
            ]
        , on "mousedown" (Json.map (TileDragStart tile) Mouse.position)
          -- , draggable "false"
        ]
        [ case model.dragTile of
            Just dragTile ->
                viewDraggingTile model dragTile tile

            Nothing ->
                text ""
        ]


viewCreatedTiles : Model -> Html Msg
viewCreatedTiles model =
    div [ class "created-tiles" ]
        (List.map (viewCreatedTile model) model.createdTiles)


view : Model -> Html Msg
view model =
    div [ class "map-builder" ]
        [ viewCreatedTiles model
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
        , onClick <| ToggleOptions (Position x y) tileSelector
        ]
        [ if showOptions then
            viewTileSelector model (Position x y) tileSelector
          else
            div [] []
        ]


viewTileSelector : Model -> Position -> TileSelector -> Html Msg
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


px : Int -> String
px number =
    toString number ++ "px"


(=>) =
    (,)


getPosition : Position -> Position -> Position -> Position
getPosition mouse start current =
    Position (mouse.x + current.x - start.x)
        (mouse.y + current.y - start.y)


viewDraggingTile : Model -> DragTile -> Tile -> Html Msg
viewDraggingTile model { start, current } tile =
    let
        realPosition =
            getPosition model.mousePosition start current
    in
        div
            [ class "gameTile"
            , style
                [ Helpers.setBackgroundAsSprite tile.image
                , "left" => px realPosition.x
                , "top" => px realPosition.y
                , "position" => "absolute"
                , "cursor" => "move"
                ]
            ]
            []
