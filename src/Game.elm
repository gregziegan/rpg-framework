module Game exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class, style)
import Matrix exposing (Matrix)
import Array exposing (Array)
import Map exposing (Tile, initTile)
import Helpers


-- | y
-- |
-- ___ x


type alias Model =
    { gameMap : Matrix Tile
    , player : Player
    , currentPosition : Position
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Player =
    { name : String
    , sprite : String
    }


initGameBoard : Matrix Tile
initGameBoard =
    Matrix.repeat 5 5 <| initTile "grass" "./assets/PathAndObjects.png"


init : Model
init =
    { gameMap = initGameBoard
    , player = greg
    , currentPosition = { x = 0, y = 0 }
    }



-- type Tile
--   = Free
--   | CannotAccess
--   | Conditional


greg : Player
greg =
    { name =
        "Greg"
    , sprite = "./assets/spellun-sprite.png"
    }


type Msg
    = HandleKey Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKey keyCode ->
            let
                curPos =
                    model.currentPosition
            in
                case keyCode of
                    40 ->
                        -- Down
                        { model | currentPosition = { x = curPos.x, y = curPos.y - 1 } } ! []

                    38 ->
                        -- Up
                        { model | currentPosition = { x = curPos.x, y = curPos.y + 1 } } ! []

                    37 ->
                        -- Left
                        { model | currentPosition = { x = curPos.x - 1, y = curPos.y } } ! []

                    39 ->
                        -- Right
                        { model | currentPosition = { x = curPos.x + 1, y = curPos.y } } ! []

                    _ ->
                        model ! []


view : Model -> Html Msg
view model =
    div []
        [ viewToolbar model
        , viewGame model
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    div
        [-- , button [ onClick (goToPage "map-builder")
        ]
        []


viewGame : Model -> Html Msg
viewGame model =
    div [ class "gameBoard" ]
        [ (model.gameMap
            |> Matrix.indexedMap (viewTile model)
            |> Map.matrixToDivs
          )
        ]


viewTile : Model -> Int -> Int -> Tile -> Html Msg
viewTile model x y tile =
    div
        [ class "gameTile"
        , style
            [ Helpers.setBackgroundAsSprite tile.image
            ]
        ]
        [ if x == model.currentPosition.x && y == model.currentPosition.y then
            div [ class "player", style [ Helpers.setBackgroundAsSprite model.player.sprite ] ] []
          else
            div [] []
        ]
