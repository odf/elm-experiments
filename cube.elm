module Main exposing (main)

import AnimationFrame
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Mouse
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)


type alias Model =
    { time : Float
    , mousePos : Mouse.Position
    , mesh : Mesh Vertex
    }


type Msg
    = FrameMsg Time
    | MouseMsg Mouse.Position


frameWidth : Int
frameWidth =
    750


frameHeight : Int
frameHeight =
    500


init : ( Model, Cmd Msg )
init =
    ( { time = 0
      , mousePos = { x = frameWidth // 2, y = frameHeight // 2 }
      , mesh = cubeMesh
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width frameWidth
        , height frameHeight
        , style [ ( "display", "block" ), ( "background", "black" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            model.mesh
            (uniforms model)
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.times FrameMsg
        , Mouse.moves MouseMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FrameMsg time ->
            ( { model | time = time / 1000 }, Cmd.none )

        MouseMsg pos ->
            ( { model | mousePos = pos }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type alias Vertex =
    { color : Vec3
    , pos : Vec3
    , posUV : Vec2
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


camera : Mouse.Position -> Mat4
camera pos =
    let
        camX =
            pos.x * 2 - frameWidth |> toFloat

        camY =
            frameHeight - pos.y * 2 |> toFloat

        cameraPos =
            vec3 camX camY 400 |> Vec3.normalize |> Vec3.scale 5
    in
        Mat4.makeLookAt cameraPos (vec3 0 0 0) (vec3 0 1 0)


uniforms : Model -> Uniforms
uniforms model =
    { rotation = (Mat4.makeRotate (0.1 * model.time) (vec3 0 1 0))
    , perspective = Mat4.makePerspective 45 (3 / 2) 0.01 100
    , camera = camera model.mousePos
    }



-- Mesh


cubeMesh : Mesh Vertex
cubeMesh =
    let
        rft =
            vec3 1 1 1

        lft =
            vec3 -1 1 1

        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
        [ face Color.green rft rfb rbb rbt
        , face Color.blue rft rfb lfb lft
        , face Color.yellow rft lft lbt rbt
        , face Color.red rfb lfb lbb rbb
        , face Color.purple lft lfb lbb lbt
        , face Color.orange rbt rbb lbb lbt
        ]
            |> List.concat
            |> WebGL.triangles


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    Color.toRgb rawColor
            in
                vec3
                    (toFloat c.red / 255)
                    (toFloat c.green / 255)
                    (toFloat c.blue / 255)

        vertex position u v =
            Vertex color position (vec2 u v)
    in
        [ ( vertex a 0 0, vertex b 1 0, vertex c 1 1 )
        , ( vertex c 1 1, vertex d 0 1, vertex a 0 0 )
        ]



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
