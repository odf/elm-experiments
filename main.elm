module Main exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Task
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)
import Window
import Cube exposing (cube, Vertex)
import Camera


type alias Model =
    { time : Float
    , size : Window.Size
    , cameraModel : Camera.Model
    , mesh : Mesh Vertex
    }


type Msg
    = FrameMsg Time
    | ResizeMsg Window.Size
    | CameraMsg Camera.Msg


init : ( Model, Cmd Msg )
init =
    ( { time = 0
      , size = { width = 0, height = 0 }
      , cameraModel = Camera.initialModel
      , mesh = cube
      }
    , Task.perform ResizeMsg Window.size
    )


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width model.cameraModel.size.width
        , height model.cameraModel.size.height
        , style [ ( "display", "block" ), ( "background", "black" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            model.mesh
            (uniforms model)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes ResizeMsg
        , AnimationFrame.times FrameMsg
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
        FrameMsg time ->
            ( { model | time = time / 1000 }, Cmd.none )

        ResizeMsg size ->
            updateCamera (Camera.resizeMsg size) model

        CameraMsg camMsg ->
            updateCamera camMsg model


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type alias Uniforms =
    { rotation : Mat4
    , viewing : Mat4
    }


type alias Varyings =
    { vcolor : Vec3
    , vposUV : Vec2
    }


uniforms : Model -> Uniforms
uniforms model =
    { rotation = (Mat4.makeRotate (0.1 * model.time) (vec3 0 1 0))
    , viewing = Camera.viewingMatrix model.cameraModel
    }



-- Shaders


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|

    attribute vec3 color;
    attribute vec3 pos;
    attribute vec2 posUV;
    uniform mat4 rotation;
    uniform mat4 viewing;
    varying vec3 vcolor;
    varying vec2 vposUV;

    void main () {
        vcolor = color;
        vposUV = posUV;
        gl_Position = viewing * rotation * vec4(pos, 1.0);
    }

    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

    precision mediump float;
    varying vec3 vcolor;
    varying vec2 vposUV;

    float PI=3.1415926535;

    void main () {
        float f1 = sin((vposUV.x + vposUV.y) * 3.0 * PI);
        float f2 = sin((vposUV.x - vposUV.y) * 3.0 * PI);
        float f = (sin(sin(f1 * f2 * 3.0) * 3.0) * 0.2) + 0.8;
        gl_FragColor = vec4(f * vcolor, 1.0);
    }

    |]
