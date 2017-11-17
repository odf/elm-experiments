module Main exposing (main)

{-
   Rotating cube with colored sides.
-}

import AnimationFrame
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)


type alias Model =
    Float


type alias Msg =
    Float


init =
    ( 0, Cmd.none )


view : Model -> Html Time
view theta =
    WebGL.toHtml
        [ width 750
        , height 500
        , style [ ( "display", "block" ), ( "background", "black" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            cubeMesh
            (uniforms theta)
        ]


subscriptions : Model -> Sub Time
subscriptions _ =
    AnimationFrame.diffs Basics.identity


update : Msg -> Model -> ( Model, Cmd Msg )
update dt theta =
    ( theta + dt / 1000, Cmd.none )


main : Program Never Float Time
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


type alias Varyings =
    { vcolor : Vec3 }


uniforms : Float -> Uniforms
uniforms theta =
    { rotation = (Mat4.makeRotate -theta (vec3 0 1 0))
    , perspective = Mat4.makePerspective 45 (3 / 2) 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
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

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- Shaders


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|

    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 perspective;
    uniform mat4 camera;
    uniform mat4 rotation;
    varying vec3 vcolor;

    void main () {
        gl_Position = perspective * camera * rotation * vec4(position, 1.0);
        vcolor = color;
    }

    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

    precision mediump float;
    uniform float shade;
    varying vec3 vcolor;

    void main () {
        gl_FragColor = shade * vec4(vcolor, 1.0);
    }

    |]
