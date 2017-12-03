module WheelEvent exposing (onMouseWheel)

import Html exposing (Attribute)
import Html.Events exposing (Options, onWithOptions)
import Json.Decode as Json


onMouseWheel : (Float -> msg) -> Attribute msg
onMouseWheel tagger =
    let
        options =
            { stopPropagation = True, preventDefault = True }

        decoder =
            Json.at [ "deltaY" ] Json.float
    in
        onWithOptions "wheel" options (Json.map tagger decoder)
