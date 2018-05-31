module Camera
    exposing
        ( Model
        , Msg(ResizeMsg, LookAtMsg, MouseDownMsg, WheelMsg)
        , initialModel
        , subscriptions
        , update
        , perspectiveMatrix
        , viewingMatrix
        , cameraDistance
        )

import AnimationFrame
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Mouse
import Keyboard
import Time exposing (Time)


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


type alias ModelData =
    { size : Size
    , origin : Position
    , cameraDistance : Float
    , fieldOfView : Float
    , dragging : Bool
    , moving : Bool
    , ndcPos : Position
    , shift : Vec3
    , rotation : Mat4
    , deltaRot : Mat4
    , moved : Bool
    , modifiers : Modifiers
    }


type Model
    = Model ModelData


type Msg
    = FrameMsg Time
    | ResizeMsg Size
    | LookAtMsg Vec3 Vec3
    | MouseMoveMsg Mouse.Position
    | MouseUpMsg Mouse.Position
    | MouseDownMsg
    | KeyDownMsg Int
    | KeyUpMsg Int
    | WheelMsg Float


initialModel : Model
initialModel =
    Model
        { size = { width = 0, height = 0 }
        , origin = { x = 0, y = 0 }
        , cameraDistance = 5
        , fieldOfView = 45
        , dragging = False
        , moving = False
        , ndcPos = { x = 0, y = 0 }
        , rotation = Mat4.identity
        , deltaRot = Mat4.identity
        , moved = False
        , modifiers = { shift = False, ctrl = False }
        , shift = vec3 0 0 0
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        FrameMsg time ->
            frameUpdate time (Model model)

        ResizeMsg size ->
            Model { model | size = size } ! []

        LookAtMsg axis up ->
            let
                rotation =
                    Mat4.makeLookAt (vec3 0 0 0) axis up
            in
                Model { model | rotation = rotation } ! []

        MouseMoveMsg pos ->
            mouseMoveUpdate pos (Model model)

        MouseDownMsg ->
            Model { model | dragging = True, moving = True, moved = False } ! []

        MouseUpMsg pos ->
            Model { model | dragging = False, moving = model.moved } ! []

        KeyUpMsg keyCode ->
            Model
                { model
                    | modifiers = updateModifiers keyCode False model.modifiers
                }
                ! []

        KeyDownMsg keyCode ->
            Model
                { model
                    | modifiers = updateModifiers keyCode True model.modifiers
                }
                ! []

        WheelMsg value ->
            wheelUpdate value (Model model)


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    let
        animation =
            if model.moving then
                [ AnimationFrame.times FrameMsg ]
            else
                []
    in
        Sub.batch <|
            animation
                ++ [ Mouse.moves MouseMoveMsg
                   , Mouse.ups MouseUpMsg
                   , Keyboard.downs KeyDownMsg
                   , Keyboard.ups KeyUpMsg
                   ]


updateModifiers : Int -> Bool -> Modifiers -> Modifiers
updateModifiers keyCode value oldMods =
    if keyCode == 16 then
        { oldMods | shift = value }
    else if keyCode == 17 then
        { oldMods | ctrl = value }
    else
        oldMods


frameUpdate : Float -> Model -> ( Model, Cmd Msg )
frameUpdate float (Model model) =
    if model.dragging then
        Model { model | moved = False } ! []
    else if model.moving then
        let
            rotation =
                orthonormalized <| Mat4.mul model.deltaRot model.rotation
        in
            Model { model | rotation = rotation } ! []
    else
        Model model ! []


mouseMoveUpdate : Mouse.Position -> Model -> ( Model, Cmd Msg )
mouseMoveUpdate pos (Model model) =
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
                panMouse ndcPos (Model model)
            else
                rotateMouse ndcPos (Model model)
        else
            Model { model | ndcPos = ndcPos } ! []


wheelUpdate : Float -> Model -> ( Model, Cmd Msg )
wheelUpdate value (Model model) =
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
        Model newModel ! []


panMouse : Position -> Model -> ( Model, Cmd Msg )
panMouse ndcPosNew (Model model) =
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
        Model
            { model
                | ndcPos = ndcPosNew
                , moved = False
                , shift = Vec3.add model.shift shift
            }
            ! []


rotateMouse : Position -> Model -> ( Model, Cmd Msg )
rotateMouse ndcPosNew (Model model) =
    let
        ( axis, angle ) =
            rotationParameters ndcPosNew model.ndcPos

        deltaRot =
            Mat4.makeRotate angle axis

        rotation =
            orthonormalized <| Mat4.mul deltaRot model.rotation
    in
        Model
            { model
                | ndcPos = ndcPosNew
                , deltaRot = deltaRot
                , rotation = rotation
                , moved = angle /= 0
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


projection : Vec3 -> Vec3 -> Vec3
projection v n =
    Vec3.scale (Vec3.dot v n) v


orthonormalized : Mat4 -> Mat4
orthonormalized m =
    let
        b1 =
            Mat4.transform m <| vec3 1 0 0

        b2 =
            Mat4.transform m <| vec3 0 1 0

        b3 =
            Mat4.transform m <| vec3 0 0 1

        n1 =
            Vec3.normalize b1

        n2 =
            Vec3.normalize
                (Vec3.sub b2 (projection b2 n1))

        n3 =
            Vec3.normalize
                (Vec3.sub b3 (Vec3.add (projection b3 n1) (projection b3 n2)))
    in
        Mat4.makeBasis n1 n2 n3


viewingMatrix : Model -> Mat4
viewingMatrix (Model model) =
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
perspectiveMatrix (Model model) =
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


cameraDistance : Model -> Float
cameraDistance (Model model) =
    model.cameraDistance
