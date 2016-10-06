module Map exposing (..)

import Html
import Html.Attributes exposing (class)
import Array
import Matrix exposing (Matrix)


type alias Tile =
    { name : String
    , image : String
    , isAccessible : Bool
    }


initTile : String -> String -> Tile
initTile name image =
    { name = name
    , image = image
    , isAccessible = True
    }


matrixToDivs : Matrix (Html.Html msg) -> Html.Html msg
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
