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
    { size : Window.Size
    , time : Float
    , cameraModel : Camera.Model
    , mesh : Mesh Vertex
    }


type Msg
    = ResizeMsg Window.Size
    | FrameMsg Time
    | CameraMsg Camera.Msg


init : ( Model, Cmd Msg )
init =
    ( { size = { width = 0, height = 0 }
      , time = 0
      , cameraModel = Camera.initialModel
      , mesh = cube
      }
    , Task.perform ResizeMsg Window.size
    )


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width model.size.width
        , height model.size.height
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeMsg size ->
            ( { model | size = size }, Cmd.none )

        FrameMsg time ->
            ( { model | time = time / 1000 }, Cmd.none )

        CameraMsg msg ->
            let
                ( updatedCameraModel, cmd ) =
                    Camera.update msg model.cameraModel
            in
                ( { model | cameraModel = updatedCameraModel }
                , Cmd.map CameraMsg cmd
                )


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
    , perspective : Mat4
    , camera : Mat4
    }


type alias Varyings =
    { vcolor : Vec3
    , vposUV : Vec2
    }


camera : Model -> Mat4
camera model =
    let
        mousePos =
            model.cameraModel.mousePos

        camX =
            mousePos.x * 2 - model.size.width |> toFloat

        camY =
            model.size.height - mousePos.y * 2 |> toFloat

        cameraPos =
            vec3 camX camY 400 |> Vec3.normalize |> Vec3.scale 5
    in
        Mat4.makeLookAt cameraPos (vec3 0 0 0) (vec3 0 1 0)


uniforms : Model -> Uniforms
uniforms model =
    let
        aspectRatio =
            (toFloat model.size.width) / (toFloat model.size.height)
    in
        { rotation = (Mat4.makeRotate (0.1 * model.time) (vec3 0 1 0))
        , perspective = Mat4.makePerspective 45 aspectRatio 0.01 100
        , camera = camera model
        }



-- Shaders


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|

    attribute vec3 color;
    attribute vec3 pos;
    attribute vec2 posUV;
    uniform mat4 perspective;
    uniform mat4 camera;
    uniform mat4 rotation;
    varying vec3 vcolor;
    varying vec2 vposUV;

    void main () {
        vcolor = color;
        vposUV = posUV;
        gl_Position = perspective * camera * rotation * vec4(pos, 1.0);
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
