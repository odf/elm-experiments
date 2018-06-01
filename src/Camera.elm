module Camera
    exposing
        ( State
        , Msg(..)
        , initialState
        , update
        , perspectiveMatrix
        , viewingMatrix
        , cameraDistance
        , isMoving
        )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Mouse
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


type alias StateData =
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


type State
    = State StateData


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


initialState : State
initialState =
    State
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


update : Msg -> State -> State
update msg (State state) =
    case msg of
        FrameMsg time ->
            frameUpdate time (State state)

        ResizeMsg size ->
            State { state | size = size }

        LookAtMsg axis up ->
            let
                rotation =
                    Mat4.makeLookAt (vec3 0 0 0) axis up
            in
                State { state | rotation = rotation }

        MouseMoveMsg pos ->
            mouseMoveUpdate pos (State state)

        MouseDownMsg ->
            State { state | dragging = True, moving = True, moved = False }

        MouseUpMsg pos ->
            State { state | dragging = False, moving = state.moved }

        KeyUpMsg keyCode ->
            State
                { state
                    | modifiers = updateModifiers keyCode False state.modifiers
                }

        KeyDownMsg keyCode ->
            State
                { state
                    | modifiers = updateModifiers keyCode True state.modifiers
                }

        WheelMsg value ->
            wheelUpdate value (State state)


updateModifiers : Int -> Bool -> Modifiers -> Modifiers
updateModifiers keyCode value oldMods =
    if keyCode == 16 then
        { oldMods | shift = value }
    else if keyCode == 17 then
        { oldMods | ctrl = value }
    else
        oldMods


frameUpdate : Float -> State -> State
frameUpdate float (State state) =
    if state.dragging then
        State { state | moved = False }
    else if state.moving then
        let
            rotation =
                orthonormalized <| Mat4.mul state.deltaRot state.rotation
        in
            State { state | rotation = rotation }
    else
        State state


mouseMoveUpdate : Mouse.Position -> State -> State
mouseMoveUpdate pos (State state) =
    let
        xRelative =
            ((toFloat pos.x) - state.origin.x) / state.size.width

        yRelative =
            ((toFloat pos.y) - state.origin.y) / state.size.height

        ndcPos =
            { x = 2 * xRelative - 1, y = 1 - 2 * yRelative }
    in
        if state.dragging then
            if state.modifiers.shift then
                panMouse ndcPos (State state)
            else
                rotateMouse ndcPos (State state)
        else
            State { state | ndcPos = ndcPos }


wheelUpdate : Float -> State -> State
wheelUpdate value (State state) =
    let
        factor =
            if value > 0 then
                0.9
            else if value < 0 then
                1 / 0.9
            else
                1.0

        newState =
            if state.modifiers.shift then
                { state | fieldOfView = factor * state.fieldOfView }
            else
                { state | cameraDistance = factor * state.cameraDistance }
    in
        State newState


panMouse : Position -> State -> State
panMouse ndcPosNew (State state) =
    let
        dx =
            ndcPosNew.x - state.ndcPos.x

        dy =
            ndcPosNew.y - state.ndcPos.y

        invRot =
            Mat4.inverseOrthonormal state.rotation

        shift =
            Mat4.transform invRot <| vec3 dx dy 0
    in
        State
            { state
                | ndcPos = ndcPosNew
                , moved = False
                , shift = Vec3.add state.shift shift
            }


rotateMouse : Position -> State -> State
rotateMouse ndcPosNew (State state) =
    let
        ( axis, angle ) =
            rotationParameters ndcPosNew state.ndcPos

        deltaRot =
            Mat4.makeRotate angle axis

        rotation =
            orthonormalized <| Mat4.mul deltaRot state.rotation
    in
        State
            { state
                | ndcPos = ndcPosNew
                , deltaRot = deltaRot
                , rotation = rotation
                , moved = angle /= 0
            }


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


viewingMatrix : State -> Mat4
viewingMatrix (State state) =
    let
        camVector =
            vec3 0 0 state.cameraDistance

        camMatrix =
            Mat4.makeLookAt camVector (vec3 0 0 0) (vec3 0 1 0)

        shift =
            Mat4.makeTranslate state.shift
    in
        Mat4.mul camMatrix <| Mat4.mul state.rotation shift


perspectiveMatrix : State -> Mat4
perspectiveMatrix (State state) =
    let
        aspectRatio =
            state.size.width / state.size.height

        fov =
            state.fieldOfView

        fovy =
            if aspectRatio >= 1 then
                fov
            else
                atan (tan (degrees (fov / 2)) / aspectRatio) * 360 / pi
    in
        Mat4.makePerspective fovy aspectRatio 0.01 100


cameraDistance : State -> Float
cameraDistance (State state) =
    state.cameraDistance


isMoving : State -> Bool
isMoving (State state) =
    state.moving
