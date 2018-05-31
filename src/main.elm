module Main exposing (main)

import Char
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Keyboard
import Math.Vector3 exposing (vec3)
import Task
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
    , cameraModel : Camera.Model
    , mesh : WebGL.Mesh Renderer.Vertex
    , material : Renderer.Material
    }


type Msg
    = ResizeMsg Window.Size
    | KeyPressMsg Char.KeyCode
    | CameraMsg Camera.Msg


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


init : ( Model, Cmd Msg )
init =
    ( { size = { width = 0, height = 0 }
      , cameraModel = Camera.initialModel
      , mesh = mesh embedder graph
      , material = initMaterial
      }
    , Task.perform ResizeMsg Window.size
    )


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes ResizeMsg
        , Keyboard.presses KeyPressMsg
        , Sub.map CameraMsg <| Camera.subscriptions model.cameraModel
        ]


updateCamera : Camera.Msg -> Model -> ( Model, Cmd Msg )
updateCamera camMsg model =
    { model | cameraModel = Camera.update camMsg model.cameraModel } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeMsg size ->
            let
                sz =
                    { width = toFloat size.width
                    , height = toFloat size.height
                    }
            in
                updateCamera (Camera.ResizeMsg sz) { model | size = size }

        KeyPressMsg code ->
            let
                char =
                    Char.toLower <| Char.fromCode code

                lookAlong axis up =
                    updateCamera (Camera.LookAtMsg axis up) model
            in
                case char of
                    'a' ->
                        lookAlong (vec3 0 -1 -1) (vec3 0 1 0)

                    'b' ->
                        lookAlong (vec3 -1 0 -1) (vec3 0 1 0)

                    'c' ->
                        lookAlong (vec3 -1 -1 0) (vec3 0 1 0)

                    'd' ->
                        lookAlong (vec3 -1 -1 -1) (vec3 0 1 0)

                    'x' ->
                        lookAlong (vec3 -1 0 0) (vec3 0 1 0)

                    'y' ->
                        lookAlong (vec3 0 -1 0) (vec3 0 0 -1)

                    'z' ->
                        lookAlong (vec3 0 0 -1) (vec3 0 1 0)

                    _ ->
                        model ! []

        CameraMsg camMsg ->
            updateCamera camMsg model


view : Model -> Html Msg
view model =
    let
        entities =
            [ Renderer.entity
                model.mesh
                model.material
                (Camera.cameraDistance model.cameraModel)
                (Camera.viewingMatrix model.cameraModel)
                (Camera.perspectiveMatrix model.cameraModel)
            ]
    in
        WebGL.toHtml
            [ Html.Attributes.width model.size.width
            , Html.Attributes.height model.size.height
            , Html.Attributes.style
                [ ( "display", "block" ), ( "background", "black" ) ]
            , Html.Events.onMouseDown (CameraMsg Camera.MouseDownMsg)
            , WheelEvent.onMouseWheel (CameraMsg << Camera.WheelMsg)
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
