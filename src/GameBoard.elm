module GameBoard exposing (..)


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
