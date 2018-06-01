module Main exposing (main)

import AnimationFrame
import Char
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Keyboard
import Mouse
import Math.Vector3 exposing (vec3)
import Task
import Time exposing (Time)
import WebGL
import Window
import SurfaceGraph exposing (Graph)
import GraphExamples
import Embed
import Camera
import Renderer
import WheelEvent


type alias Model =
    { size : Window.Size
    , cameraState : Camera.State
    , mesh : WebGL.Mesh Renderer.Vertex
    , material : Renderer.Material
    , modifiers : { shift : Bool, ctrl : Bool }
    }


type Msg
    = ResizeMsg Window.Size
    | FrameMsg Time
    | MouseUpMsg Mouse.Position
    | MouseDownMsg
    | MouseMoveMsg Mouse.Position
    | KeyDownMsg Int
    | KeyUpMsg Int
    | WheelMsg Float


init : ( Model, Cmd Msg )
init =
    ( { size = { width = 0, height = 0 }
      , cameraState = Camera.initialState
      , mesh = mesh embedder graph
      , material = initMaterial
      , modifiers = { shift = False, ctrl = False }
      }
    , Task.perform ResizeMsg Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animation =
            if Camera.isMoving model.cameraState then
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
                   , Window.resizes ResizeMsg
                   ]


updateCameraState : (Camera.State -> Camera.State) -> Model -> ( Model, Cmd Msg )
updateCameraState fn model =
    { model | cameraState = fn model.cameraState } ! []


setModifiers : Int -> Bool -> Model -> Model
setModifiers keyCode value model =
    let
        oldModifiers =
            model.modifiers
    in
        if keyCode == 16 then
            { model | modifiers = { oldModifiers | shift = value } }
        else if keyCode == 17 then
            { model | modifiers = { oldModifiers | ctrl = value } }
        else
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeMsg size ->
            updateCameraState (Camera.setFrameSize size) { model | size = size }

        FrameMsg time ->
            updateCameraState (Camera.nextFrame time) model

        MouseDownMsg ->
            updateCameraState Camera.startDragging model

        MouseUpMsg pos ->
            updateCameraState (Camera.finishDragging pos) model

        MouseMoveMsg pos ->
            let
                alter =
                    model.modifiers.shift
            in
                updateCameraState (Camera.setMousePosition pos alter) model

        WheelMsg val ->
            let
                alter =
                    model.modifiers.shift
            in
                updateCameraState (Camera.updateZoom val alter) model

        KeyDownMsg code ->
            setModifiers code True model ! []

        KeyUpMsg code ->
            let
                tmpModel =
                    setModifiers code False model

                char =
                    Char.toLower <| Char.fromCode code
            in
                case char of
                    'a' ->
                        updateCameraState
                            (Camera.lookAlong (vec3 0 -1 -1) (vec3 0 1 0))
                            tmpModel

                    'b' ->
                        updateCameraState
                            (Camera.lookAlong (vec3 -1 0 -1) (vec3 0 1 0))
                            tmpModel

                    'c' ->
                        updateCameraState
                            (Camera.lookAlong (vec3 -1 -1 0) (vec3 0 1 0))
                            tmpModel

                    'd' ->
                        updateCameraState
                            (Camera.lookAlong (vec3 -1 -1 -1) (vec3 0 1 0))
                            tmpModel

                    'x' ->
                        updateCameraState
                            (Camera.lookAlong (vec3 -1 0 0) (vec3 0 1 0))
                            tmpModel

                    'y' ->
                        updateCameraState
                            (Camera.lookAlong (vec3 0 -1 0) (vec3 0 0 -1))
                            tmpModel

                    'z' ->
                        updateCameraState
                            (Camera.lookAlong (vec3 0 0 -1) (vec3 0 1 0))
                            tmpModel

                    _ ->
                        tmpModel ! []


view : Model -> Html Msg
view model =
    let
        entities =
            [ Renderer.entity
                model.mesh
                model.material
                (Camera.cameraDistance model.cameraState)
                (Camera.viewingMatrix model.cameraState)
                (Camera.perspectiveMatrix model.cameraState)
            ]
    in
        WebGL.toHtml
            [ Html.Attributes.width model.size.width
            , Html.Attributes.height model.size.height
            , Html.Attributes.style
                [ ( "display", "block" ), ( "background", "black" ) ]
            , Html.Events.onMouseDown MouseDownMsg
            , WheelEvent.onMouseWheel WheelMsg
            ]
            entities


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


graph : Maybe SurfaceGraph.Graph
graph =
    GraphExamples.fulleroidI_5_12


embedder : Embed.Embedder
embedder =
    Embed.molecular


mesh : Embed.Embedder -> Maybe Graph -> WebGL.Mesh Renderer.Vertex
mesh embedder gr =
    case gr of
        Nothing ->
            WebGL.lines []

        Just adj ->
            let
                pos =
                    embedder adj

                meshVertex v =
                    let
                        p =
                            Embed.getPos v pos
                    in
                        { color = (vec3 1 1 1), pos = p, normal = p }

                meshEdge ( v, w ) =
                    ( meshVertex v, meshVertex w )
            in
                WebGL.lines <| List.map meshEdge <| SurfaceGraph.edges adj


initMaterial : Renderer.Material
initMaterial =
    { ambientColor = vec3 1 1 2
    , diffuseColor = vec3 1 1 1
    , specularColor = vec3 1 1 1
    , ka = 0.1
    , kd = 1.0
    , ks = 0.2
    , shininess = 20.0
    }
