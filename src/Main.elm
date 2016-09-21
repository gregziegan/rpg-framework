module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as Html
import Keyboard
import Navigation


-- import UrlParser as P

import Game


-- | y
-- |
-- ___ x


type alias Model =
    { gamePage : Game.Model
    , url : String
    }


init : Maybe Location -> ( Model, Cmd Msg )
init location =
    ( { gamePage = Game.init
      , url = "#game"
      }
    , Cmd.none
    )


type Location
    = Game
    | MapBuilder


urlFor : Location -> String
urlFor loc =
    let
        url =
            case loc of
                Game ->
                    "game"

                MapBuilder ->
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
                Just Game

            "#map-builder" ->
                Just MapBuilder

            _ ->
                Just Game


type Msg
    = GamePage Game.Msg


urlUpdate : Maybe Location -> Model -> ( Model, Cmd Msg )
urlUpdate location oldModel =
    let
        newModel =
            case Maybe.withDefault Game location of
                Game ->
                    { oldModel | url = "#game" }

                MapBuilder ->
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


view : Model -> Html Msg
view model =
    div []
        [ Html.map GamePage <| Game.view model.gamePage ]



-- program
--     :  Parser data
--     -> { init : data -> (model, Cmd msg)
--        , update : msg -> model -> (model, Cmd msg)
--        , urlUpdate : data -> model -> (model, Cmd msg)
--        , view : model -> Html msg,
--        subscriptions : model -> Sub msg }
--     -> Program Never
-- This function augments Html.App.program. The new things include:
--
-- Parser — Whenever this library changes the URL, the parser you provide will run. This turns the raw URL string into useful data.
--
-- urlUpdate — Whenever the Parser produces new data, we need to update our model in some way to react to the change. The urlUpdate function handles this case. (It works exactly like the normal update function. Take in a message, update the model.)
--
-- Note: The urlUpdate function is called every time the URL changes. This includes things exposed by this library, like back and newUrl, as well as whenever the user clicks the back or forward buttons of the browsers. If the address changes, you should hear about it.


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
