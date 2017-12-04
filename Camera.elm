module Camera
    exposing
        ( Model
        , Msg
        , resizeMsg
        , initialModel
        , subscriptions
        , update
        , view
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
import Keyboard
import Time exposing (Time)
import WebGL
import WheelEvent


type alias Size =
    { width : Float
    , height : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Modifiers =
    { shift : Bool
    , ctrl : Bool
    }


type alias Model =
    { size : Size
    , origin : Position
    , cameraDistance : Float
    , fieldOfView : Float
    , dragging : Bool
    , ndcPos : Position
    , shift : Vec3
    , rotation : Mat4
    , deltaRot : Mat4
    , moved : Bool
    , modifiers : Modifiers
    }


type Msg
    = FrameMsg Time
    | ResizeMsg Size
    | MouseMoveMsg Mouse.Position
    | MouseUpMsg Mouse.Position
    | MouseDownMsg
    | KeyDownMsg Int
    | KeyUpMsg Int
    | WheelMsg Float


resizeMsg : Size -> Msg
resizeMsg size =
    ResizeMsg size


initialModel : Model
initialModel =
    { size = { width = 0, height = 0 }
    , origin = { x = 0, y = 0 }
    , cameraDistance = 5
    , fieldOfView = 45
    , dragging = False
    , ndcPos = { x = 0, y = 0 }
    , rotation = Mat4.identity
    , deltaRot = Mat4.identity
    , moved = False
    , modifiers = { shift = False, ctrl = False }
    , shift = vec3 0 0 0
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

        KeyUpMsg keyCode ->
            { model
                | modifiers = updateModifiers keyCode False model.modifiers
            }
                ! []

        KeyDownMsg keyCode ->
            { model
                | modifiers = updateModifiers keyCode True model.modifiers
            }
                ! []

        WheelMsg value ->
            wheelUpdate value model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.times FrameMsg
        , Mouse.moves MouseMoveMsg
        , Mouse.ups MouseUpMsg
        , Keyboard.downs KeyDownMsg
        , Keyboard.ups KeyUpMsg
        ]


view : List WebGL.Entity -> Model -> Html Msg
view entities model =
    WebGL.toHtml
        [ width (round model.size.width)
        , height (round model.size.height)
        , style [ ( "display", "block" ), ( "background", "black" ) ]
        , Html.Events.onMouseDown MouseDownMsg
        , WheelEvent.onMouseWheel WheelMsg
        ]
        entities


updateModifiers : Int -> Bool -> Modifiers -> Modifiers
updateModifiers keyCode value oldMods =
    if keyCode == 16 then
        { oldMods | shift = value }
    else if keyCode == 17 then
        { oldMods | ctrl = value }
    else
        oldMods


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
    let
        xRelative =
            ((toFloat pos.x) - model.origin.x) / model.size.width

        yRelative =
            ((toFloat pos.y) - model.origin.y) / model.size.height

        ndcPos =
            { x = 2 * xRelative - 1, y = 1 - 2 * yRelative }
    in
        if model.dragging then
            if model.modifiers.shift then
                panMouse ndcPos model
            else
                rotateMouse ndcPos model
        else
            { model | ndcPos = ndcPos } ! []


wheelUpdate : Float -> Model -> ( Model, Cmd Msg )
wheelUpdate value model =
    let
        factor =
            if value > 0 then
                0.9
            else if value < 0 then
                1 / 0.9
            else
                1.0

        newModel =
            if model.modifiers.shift then
                { model | fieldOfView = factor * model.fieldOfView }
            else
                { model | cameraDistance = factor * model.cameraDistance }
    in
        newModel ! []


panMouse : Position -> Model -> ( Model, Cmd Msg )
panMouse ndcPosNew model =
    let
        dx =
            ndcPosNew.x - model.ndcPos.x

        dy =
            ndcPosNew.y - model.ndcPos.y

        invRot =
            Mat4.inverseOrthonormal model.rotation

        shift =
            Mat4.transform invRot <| vec3 dx dy 0
    in
        { model
            | ndcPos = ndcPosNew
            , moved = dx /= 0 || dy /= 0
            , shift = Vec3.add model.shift shift
        }
            ! []


rotateMouse : Position -> Model -> ( Model, Cmd Msg )
rotateMouse ndcPosNew model =
    let
        ( axis, angle ) =
            rotationParameters ndcPosNew model.ndcPos

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


zRotationAngle : Float -> Float -> Float -> Float -> Float
zRotationAngle px py dx dy =
    if px > 0.9 then
        dy
    else if px < -0.9 then
        -dy
    else if py > 0.9 then
        -dx
    else if py < -0.9 then
        dx
    else
        0


rotationParameters : Position -> Position -> ( Vec3, Float )
rotationParameters newPos oldPos =
    let
        dx =
            (newPos.x - oldPos.x) * pi / 2

        dy =
            (newPos.y - oldPos.y) * pi / 2

        aroundZ =
            abs newPos.x > 0.9 || abs newPos.y > 0.9

        angle =
            if aroundZ then
                zRotationAngle newPos.x newPos.y dx dy
            else
                dx ^ 2 + dy ^ 2 |> sqrt

        axis =
            if angle == 0 || aroundZ then
                vec3 0 0 1
            else
                vec3 (-dy / angle) (dx / angle) 0
    in
        ( axis, angle )


viewingMatrix : Model -> Mat4
viewingMatrix model =
    let
        camVector =
            vec3 0 0 model.cameraDistance

        camMatrix =
            Mat4.makeLookAt camVector (vec3 0 0 0) (vec3 0 1 0)

        shift =
            Mat4.makeTranslate model.shift
    in
        Mat4.mul camMatrix <| Mat4.mul model.rotation shift


perspectiveMatrix : Model -> Mat4
perspectiveMatrix model =
    let
        aspectRatio =
            model.size.width / model.size.height

        fov =
            model.fieldOfView

        fovy =
            if aspectRatio >= 1 then
                fov
            else
                atan (tan (degrees (fov / 2)) / aspectRatio) * 360 / pi
    in
        Mat4.makePerspective fovy aspectRatio 0.01 100
