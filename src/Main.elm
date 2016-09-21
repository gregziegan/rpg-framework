module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as Html


-- | y
-- |
-- ___ x


type alias Model =
    { tiles : List Tile }


type alias Player =
    { name : String
    , hatColor : String
    }


type alias Tile =
    { name : String
    , backgroundImage : String
    , player : Maybe Player
    }


init : Model
init =
    { tiles = [ initTile <| Just greg, initTile Nothing, initTile Nothing, initTile Nothing, initTile Nothing ] }


initTile : Maybe Player -> Tile
initTile player =
    { name = "grass"
    , backgroundImage = "http://oi45.tinypic.com/2ir0vbl.jpg"
    , player = player
    }


greg =
    { name = "Greg"
    , hatColor = "red"
    }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        (List.map viewTile model.tiles)


viewTile : Tile -> Html Msg
viewTile tile =
    div
        [ class "gameTile"
        , style
            [ ( "background-image", "url(" ++ tile.backgroundImage ++ ")" )
            ]
        ]
        [ case tile.player of
            Just player ->
                div [ class "player", style [ ( "background-color", player.hatColor ) ] ] []

            Nothing ->
                div [] []
        ]


main : Program Never
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
