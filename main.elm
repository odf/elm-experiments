module Main exposing (main)

import Html exposing (Html)
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
    }


type Msg
    = ResizeMsg Window.Size
    | CameraMsg Camera.Msg


init : ( Model, Cmd Msg )
init =
    ( { size = { width = 0, height = 0 }
      , cameraModel = Camera.initialModel
      , mesh = Cube.cube
      }
    , Task.perform ResizeMsg Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes ResizeMsg
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
            updateCamera (Camera.resizeMsg size) model

        CameraMsg camMsg ->
            updateCamera camMsg model


view : Model -> Html Msg
view model =
    let
        entities =
            [ Renderer.entity model.mesh model.cameraModel ]
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
