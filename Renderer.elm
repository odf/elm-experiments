module Renderer exposing (Vertex, entity)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Mesh, Shader)
import Camera


type alias Vertex =
    { color : Vec3
    , pos : Vec3
    }


type alias Uniforms =
    { viewing : Mat4
    }


type alias Varyings =
    { vcolor : Vec3
    }


entity : Mesh Vertex -> Camera.Model -> WebGL.Entity
entity mesh model =
    let
        uniforms =
            { viewing = Camera.viewingMatrix model }
    in
        WebGL.entity vertexShader fragmentShader mesh uniforms


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|

    attribute vec3 color;
    attribute vec3 pos;
    uniform mat4 viewing;
    varying vec3 vcolor;

    void main () {
        vcolor = color;
        gl_Position = viewing * vec4(pos, 1.0);
    }

    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

    precision mediump float;
    varying vec3 vcolor;

    void main () {
        gl_FragColor = vec4(vcolor, 1.0);
    }

    |]
