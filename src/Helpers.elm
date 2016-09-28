module Helpers exposing (..)


setBackgroundAsSprite : String -> ( String, String )
setBackgroundAsSprite sprite =
    ( "background", "url(" ++ sprite ++ ") 0px 0px" )
