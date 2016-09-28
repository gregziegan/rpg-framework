module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.App as Html
import Keyboard
import Navigation
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)
import String
import Game
import MapBuilder


type alias Model =
    { page : Page
    , game : Game.Model
    , mapBuilder : MapBuilder.Model
    }


init : Result String Page -> ( Model, Cmd Msg )
init result =
    urlUpdate result
        { page = GamePage
        , game = Game.init
        , mapBuilder = fst <| MapBuilder.init
        }


type Page
    = GamePage
    | MapBuilderPage


toHash : Page -> String
toHash page =
    case page of
        GamePage ->
            "#game"

        MapBuilderPage ->
            "#map-builder"


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format GamePage (s "game")
        , format MapBuilderPage (s "map-builder")
        ]


type Msg
    = SetGame Game.Msg
    | SetMapBuilder MapBuilder.Msg
    | GoToPage Page


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "urlResult" result of
        Err _ ->
            ( model, Navigation.modifyUrl (toHash model.page) )

        Ok page ->
            { model | page = page } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGame gameMsg ->
            let
                ( updatedGame, commands ) =
                    Game.update gameMsg model.game
            in
                { model | game = updatedGame } ! []

        SetMapBuilder mapBuilderMsg ->
            let
                ( updatedMapBuilder, commands ) =
                    MapBuilder.update mapBuilderMsg model.mapBuilder
            in
                { model | mapBuilder = updatedMapBuilder } ! []

        GoToPage page ->
            { model | page = page } ! [ Navigation.newUrl (toHash page) ]


view : Model -> Html Msg
view model =
    Html.div []
        [ case model.page of
            GamePage ->
                Html.map SetGame <| Game.view model.game

            MapBuilderPage ->
                Html.map SetMapBuilder <| MapBuilder.view model.mapBuilder
        ]


hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


main : Program Never
main =
    Navigation.program (Navigation.makeParser hashParser)
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs (\keyCode -> SetGame (Game.HandleKey keyCode))
