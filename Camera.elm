module Camera
    exposing
        ( Model
        , Msg
        , resizeMsg
        , initialModel
        , subscriptions
        , update
        , view
        , cameraDistance
        , perspectiveMatrix
        , viewingMatrix
        )

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onMouseUp, onMouseDown)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Mouse
import Time exposing (Time)
import WebGL


type alias Size =
    { width : Float
    , height : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Model =
    { time : Float
    , size : Size
    , origin : Position
    , mouseDown : Bool
    , ndcPosOld : Position
    , ndcPosNew : Position
    }


type Msg
    = FrameMsg Time
    | ResizeMsg Size
    | MouseMoveMsg Mouse.Position
    | MouseUpMsg
    | MouseDownMsg


resizeMsg : Size -> Msg
resizeMsg size =
    ResizeMsg size


initialModel : Model
initialModel =
    { time = 0
    , size = { width = 0, height = 0 }
    , origin = { x = 0, y = 0 }
    , mouseDown = False
    , ndcPosOld = { x = 0, y = 0 }
    , ndcPosNew = { x = 0, y = 0 }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FrameMsg time ->
            { model | time = time / 1000 } ! []

        ResizeMsg size ->
            { model | size = size } ! []

        MouseMoveMsg pos ->
            { model | ndcPosNew = ndcPos pos.x pos.y model } ! []

        MouseDownMsg ->
            { model | mouseDown = True, ndcPosOld = model.ndcPosNew } ! []

        MouseUpMsg ->
            { model | mouseDown = False } ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.times FrameMsg
        , Mouse.moves MouseMoveMsg
        ]


view : List WebGL.Entity -> Model -> Html Msg
view entities model =
    WebGL.toHtml
        [ width (round model.size.width)
        , height (round model.size.height)
        , style [ ( "display", "block" ), ( "background", "black" ) ]
        , onMouseDown MouseDownMsg
        , onMouseUp MouseUpMsg
        ]
        entities


ndcPos : Int -> Int -> Model -> Position
ndcPos posX posY model =
    let
        xRelative =
            ((toFloat posX) - model.origin.x) / model.size.width

        yRelative =
            ((toFloat posY) - model.origin.y) / model.size.height
    in
        { x = 2 * xRelative - 1, y = 1 - 2 * yRelative }


cameraDistance : Float
cameraDistance =
    5.0


scaleTo : Float -> Vec3 -> Vec3
scaleTo length vec =
    vec |> Vec3.normalize |> Vec3.scale length


cameraPosition : Model -> Vec3
cameraPosition model =
    vec3 model.ndcPosNew.x model.ndcPosNew.y 1 |> scaleTo cameraDistance


viewingMatrix : Model -> Mat4
viewingMatrix model =
    let
        cameraMatrix =
            Mat4.makeLookAt (cameraPosition model) (vec3 0 0 0) (vec3 0 1 0)

        rotationMatrix =
            Mat4.makeRotate (0.1 * model.time) (vec3 0 1 0)
    in
        Mat4.mul cameraMatrix rotationMatrix


perspectiveMatrix : Model -> Mat4
perspectiveMatrix model =
    let
        aspectRatio =
            model.size.width / model.size.height

        fov =
            45

        fovy =
            if aspectRatio >= 1 then
                fov
            else
                atan (tan (degrees (fov / 2)) / aspectRatio) * 360 / pi
    in
        Mat4.makePerspective fovy aspectRatio 0.01 100
