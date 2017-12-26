module Main exposing (main)

import Char
import Html exposing (Html)
import Keyboard
import Math.Vector3 exposing (vec3)
import Task
import WebGL
import Window
import GraphMesh
import Embed
import Camera
import Renderer


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


init : ( Model, Cmd Msg )
init =
    ( { size = { width = 0, height = 0 }
      , cameraModel = Camera.initialModel
      , mesh = GraphMesh.mesh Embed.molecular GraphMesh.fulleroidI_5_12
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
    let
        ( updatedCameraModel, cmd ) =
            Camera.update camMsg model.cameraModel
    in
        ( { model | cameraModel = updatedCameraModel }
        , Cmd.map CameraMsg cmd
        )


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
                updateCamera (Camera.ResizeMsg sz) model

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
            [ Renderer.entity model.mesh model.material model.cameraModel ]
    in
        Html.map CameraMsg <| Camera.view entities model.cameraModel


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
