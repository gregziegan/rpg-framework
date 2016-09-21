module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as Html
import Keyboard
import Navigation


-- import UrlParser as P

import Game
import MapBuilder


-- | y
-- |
-- ___ x


type alias Model =
    { gamePage : Game.Model
    , mapBuilderPage : MapBuilder.Model
    , url : String
    }


init : Maybe Location -> ( Model, Cmd Msg )
init location =
    ( { gamePage = Game.init
      , mapBuilderPage = fst <| MapBuilder.init
      , url = "#map-builder"
      }
    , Cmd.none
    )


type Location
    = GameLoc
    | MapBuilderLoc


urlFor : Location -> String
urlFor loc =
    let
        url =
            case loc of
                GameLoc ->
                    "game"

                MapBuilderLoc ->
                    "map-builder"
    in
        "#" ++ url


locFor : Navigation.Location -> Maybe Location
locFor path =
    let
        segments =
            path.hash
    in
        case segments of
            "#game" ->
                Just GameLoc

            "#map-builder" ->
                Just MapBuilderLoc

            _ ->
                Just MapBuilderLoc


type Msg
    = GamePage Game.Msg
    | MapBuilder MapBuilder.Msg


urlUpdate : Maybe Location -> Model -> ( Model, Cmd Msg )
urlUpdate location oldModel =
    let
        newModel =
            case Maybe.withDefault MapBuilderLoc location of
                GameLoc ->
                    { oldModel | url = "#game" }

                MapBuilderLoc ->
                    { oldModel | url = "#map-builder" }
    in
        ( newModel
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GamePage gameMsg ->
            let
                ( updatedGame, commands ) =
                    Game.update gameMsg model.gamePage
            in
                { model | gamePage = updatedGame } ! []

        MapBuilder mapBuilderMsg ->
            let
                ( updatedMapBuilder, commands ) =
                    MapBuilder.update mapBuilderMsg model.mapBuilderPage
            in
                { model | mapBuilderPage = updatedMapBuilder } ! []


view : Model -> Html Msg
view model =
    div []
        [ Html.map GamePage <| Game.view model.gamePage ]


main : Program Never
main =
    Navigation.program (Navigation.makeParser locFor)
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs (\keyCode -> GamePage (Game.HandleKey keyCode))
