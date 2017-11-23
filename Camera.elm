module Camera
    exposing
        ( Model
        , Msg
        , resizeMsg
        , initialModel
        , subscriptions
        , update
        )

import Mouse


type alias Size =
    { width : Int
    , height : Int
    }


type alias Model =
    { size : Size
    , mousePos : Mouse.Position
    }


type Msg
    = ResizeMsg Size
    | MouseMsg Mouse.Position


resizeMsg : Size -> Msg
resizeMsg size =
    ResizeMsg size


initialModel : Model
initialModel =
    { size = { width = 0, height = 0 }
    , mousePos = { x = 0, y = 0 }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeMsg size ->
            ( { model | size = size }, Cmd.none )

        MouseMsg pos ->
            ( { model | mousePos = pos }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Mouse.moves MouseMsg
