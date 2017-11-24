module Camera
    exposing
        ( Model
        , Msg
        , resizeMsg
        , initialModel
        , subscriptions
        , update
        , viewingMatrix
        )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3)
import Mouse


type alias Size =
    { width : Int
    , height : Int
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Model =
    { size : Size
    , origin : Position
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
    , origin = { x = 0, y = 0 }
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


perspectiveMatrix : Model -> Mat4
perspectiveMatrix model =
    let
        aspectRatio =
            (toFloat model.size.width) / (toFloat model.size.height)
    in
        Mat4.makePerspective 45 aspectRatio 0.01 100


cameraMatrix : Model -> Mat4
cameraMatrix model =
    let
        camX =
            model.mousePos.x * 2 - model.size.width |> toFloat

        camY =
            model.size.height - model.mousePos.y * 2 |> toFloat

        camZ =
            toFloat <| max model.size.height model.size.width

        cameraPos =
            vec3 camX camY camZ |> Vec3.normalize |> Vec3.scale 5
    in
        Mat4.makeLookAt cameraPos (vec3 0 0 0) (vec3 0 1 0)


viewingMatrix : Model -> Mat4
viewingMatrix model =
    Mat4.mul (perspectiveMatrix model) (cameraMatrix model)
