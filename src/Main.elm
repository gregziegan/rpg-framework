module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as Html
import Keyboard


-- | y
-- |
-- ___ x


type alias Model =
    { tiles : List Tile
    , player : Player
    , currentPosition : Position
    }


type alias Position =
    Int


type alias Player =
    { name : String
    , hatColor : String
    }


type alias Tile =
    { name : String
    , backgroundImage : String
    }


init : Model
init =
    { tiles = [ initTile, initTile, initTile, initTile, initTile ]
    , player = greg
    , currentPosition = 0
    }


initTile : Tile
initTile =
    { name = "grass"
    , backgroundImage = "http://oi45.tinypic.com/2ir0vbl.jpg"
    }


greg : Player
greg =
    { name = "Greg"
    , hatColor = "red"
    }


type Msg
    = HandleKey Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKey keyCode ->
            case keyCode of
                40 ->
                    -- Down
                    { model | currentPosition = model.currentPosition - 1 } ! []

                38 ->
                    -- Up
                    { model | currentPosition = model.currentPosition + 1 } ! []

                _ ->
                    model ! []


view : Model -> Html Msg
view model =
    div []
        (List.indexedMap (\index tile -> viewTile model index tile) model.tiles)


viewTile : Model -> Position -> Tile -> Html Msg
viewTile model position tile =
    div
        [ class "gameTile"
        , style
            [ ( "background-image", "url(" ++ tile.backgroundImage ++ ")" )
            ]
        ]
        [ if position == model.currentPosition then
            div [ class "player", style [ ( "background-color", model.player.hatColor ) ] ] []
          else
            div [] []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs HandleKey


main : Program Never
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
