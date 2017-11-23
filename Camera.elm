module Camera exposing (Model, Msg, initialModel, subscriptions, update)

import Mouse


type alias Model =
    { mousePos : Mouse.Position
    }


type Msg
    = MouseMsg Mouse.Position


initialModel : Model
initialModel =
    { mousePos = { x = 0, y = 0 } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg pos ->
            ( { model | mousePos = pos }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Mouse.moves MouseMsg
