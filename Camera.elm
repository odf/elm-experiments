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
import Html.Events
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
    , dragging : Bool
    , ndcPos : Position
    , rotation : Mat4
    , deltaRot : Mat4
    , moved : Bool
    }


type Msg
    = FrameMsg Time
    | ResizeMsg Size
    | MouseMoveMsg Mouse.Position
    | MouseUpMsg Mouse.Position
    | MouseDownMsg


resizeMsg : Size -> Msg
resizeMsg size =
    ResizeMsg size


initialModel : Model
initialModel =
    { time = 0
    , size = { width = 0, height = 0 }
    , origin = { x = 0, y = 0 }
    , dragging = False
    , ndcPos = { x = 0, y = 0 }
    , rotation = Mat4.identity
    , deltaRot = Mat4.identity
    , moved = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FrameMsg time ->
            frameUpdate time model

        ResizeMsg size ->
            { model | size = size } ! []

        MouseMoveMsg pos ->
            mouseMoveUpdate pos model

        MouseDownMsg ->
            { model | dragging = True, deltaRot = Mat4.identity } ! []

        MouseUpMsg pos ->
            { model | dragging = False } ! []


frameUpdate : Float -> Model -> ( Model, Cmd Msg )
frameUpdate float model =
    if model.dragging then
        if model.moved then
            { model | moved = False } ! []
        else
            { model | deltaRot = Mat4.identity } ! []
    else
        { model | rotation = Mat4.mul model.deltaRot model.rotation } ! []


mouseMoveUpdate : Mouse.Position -> Model -> ( Model, Cmd Msg )
mouseMoveUpdate pos model =
    if model.dragging then
        dragMouse pos model
    else
        { model | ndcPos = ndcPos pos.x pos.y model } ! []


dragMouse : Mouse.Position -> Model -> ( Model, Cmd Msg )
dragMouse pos model =
    let
        ndcPosNew =
            ndcPos pos.x pos.y model

        dx =
            (ndcPosNew.x - model.ndcPos.x) * pi / 2

        dy =
            (ndcPosNew.y - model.ndcPos.y) * pi / 2

        angle =
            dx ^ 2 + dy ^ 2 |> sqrt

        axis =
            if angle == 0 then
                vec3 0 0 1
            else
                vec3 (-dy / angle) (dx / angle) 0

        deltaRot =
            Mat4.makeRotate angle axis

        rotation =
            Mat4.mul deltaRot model.rotation
    in
        { model
            | ndcPos = ndcPosNew
            , deltaRot = deltaRot
            , rotation = rotation
            , moved = angle > 0
        }
            ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.times FrameMsg
        , Mouse.moves MouseMoveMsg
        , Mouse.ups MouseUpMsg
        ]


view : List WebGL.Entity -> Model -> Html Msg
view entities model =
    WebGL.toHtml
        [ width (round model.size.width)
        , height (round model.size.height)
        , style [ ( "display", "block" ), ( "background", "black" ) ]
        , Html.Events.onMouseDown MouseDownMsg
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


cameraMatrix : Mat4
cameraMatrix =
    Mat4.makeLookAt (vec3 0 0 cameraDistance) (vec3 0 0 0) (vec3 0 1 0)


viewingMatrix : Model -> Mat4
viewingMatrix model =
    Mat4.mul cameraMatrix model.rotation


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
