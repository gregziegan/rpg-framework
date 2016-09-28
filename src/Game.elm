module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Matrix exposing (Matrix)
import Array exposing (Array)


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


type alias Tile =
    { name : String
    , image : String
    , isAccessible : Bool
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


initTile : String -> String -> Tile
initTile name image =
    { name = name
    , image = image
    , isAccessible = True
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
    div [ class "gameBoard" ]
        [ (model.gameMap
            |> Matrix.indexedMap (viewTile model)
            |> matrixToDivs
          )
        ]


setBackgroundAsSprite : String -> ( String, String )
setBackgroundAsSprite sprite =
    ( "background", "url(" ++ sprite ++ ") 0px 0px" )


viewTile : Model -> Int -> Int -> Tile -> Html Msg
viewTile model x y tile =
    div
        [ class "gameTile"
        , style
            [ setBackgroundAsSprite tile.image
            ]
        ]
        [ if x == model.currentPosition.x && y == model.currentPosition.y then
            div [ class "player", style [ setBackgroundAsSprite model.player.sprite ] ] []
          else
            div [] []
        ]


matrixToDivs : Matrix (Html.Html Msg) -> Html.Html Msg
matrixToDivs matrix =
    let
        makeRow y =
            Matrix.getRow y matrix
                |> Maybe.map (Array.toList)
                |> Maybe.withDefault []
                |> Html.div [ class "gameRow" ]

        height =
            Matrix.height matrix
    in
        [0..height]
            |> List.map makeRow
            |> Html.div []
