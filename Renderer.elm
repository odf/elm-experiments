module Renderer exposing (entity)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Mesh, Shader)
import Cube
import Camera


type alias Uniforms =
    { viewing : Mat4
    }


type alias Varyings =
    { vcolor : Vec3
    , vposUV : Vec2
    }


entity : Mesh Cube.Vertex -> Camera.Model -> WebGL.Entity
entity mesh model =
    let
        uniforms =
            { viewing = Camera.viewingMatrix model }
    in
        WebGL.entity vertexShader fragmentShader mesh uniforms


vertexShader : Shader Cube.Vertex Uniforms Varyings
vertexShader =
    [glsl|

    attribute vec3 color;
    attribute vec3 pos;
    attribute vec2 posUV;
    uniform mat4 viewing;
    varying vec3 vcolor;
    varying vec2 vposUV;

    void main () {
        vcolor = color;
        vposUV = posUV;
        gl_Position = viewing * vec4(pos, 1.0);
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
