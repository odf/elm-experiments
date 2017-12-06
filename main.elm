module Main exposing (main)

import Char
import Html exposing (Html)
import Keyboard
import Math.Vector3 exposing (vec3)
import Task
import WebGL
import Window
import Cube
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
      , mesh = Cube.cube
      , material = initMaterial
      }
    , Task.perform ResizeMsg Window.size
    )


initMaterial : Renderer.Material
initMaterial =
    { ambientColor = vec3 1 1 1
    , diffuseColor = vec3 1 1 1
    , specularColor = vec3 1 1 1
    , ka = 0.0
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
                updateCamera (Camera.resizeMsg sz) model

        KeyPressMsg code ->
            let
                char =
                    Char.toLower <| Char.fromCode code
            in
                case char of
                    'a' ->
                        updateCamera lookAlongYzMsg model

                    'b' ->
                        updateCamera lookAlongXzMsg model

                    'c' ->
                        updateCamera lookAlongXyMsg model

                    'd' ->
                        updateCamera lookAlongXyzMsg model

                    'x' ->
                        updateCamera lookAlongXMsg model

                    'y' ->
                        updateCamera lookAlongYMsg model

                    'z' ->
                        updateCamera lookAlongZMsg model

                    _ ->
                        model ! []

        CameraMsg camMsg ->
            updateCamera camMsg model


lookAlongXMsg : Camera.Msg
lookAlongXMsg =
    Camera.lookAtMsg (vec3 -1 0 0) (vec3 0 1 0)


lookAlongYMsg : Camera.Msg
lookAlongYMsg =
    Camera.lookAtMsg (vec3 0 -1 0) (vec3 0 0 -1)


lookAlongZMsg : Camera.Msg
lookAlongZMsg =
    Camera.lookAtMsg (vec3 0 0 -1) (vec3 0 1 0)


lookAlongXyMsg : Camera.Msg
lookAlongXyMsg =
    Camera.lookAtMsg (vec3 -1 -1 0) (vec3 0 1 0)


lookAlongXzMsg : Camera.Msg
lookAlongXzMsg =
    Camera.lookAtMsg (vec3 -1 0 -1) (vec3 0 1 0)


lookAlongYzMsg : Camera.Msg
lookAlongYzMsg =
    Camera.lookAtMsg (vec3 0 -1 -1) (vec3 0 1 0)


lookAlongXyzMsg : Camera.Msg
lookAlongXyzMsg =
    Camera.lookAtMsg (vec3 -1 -1 -1) (vec3 0 1 0)


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
