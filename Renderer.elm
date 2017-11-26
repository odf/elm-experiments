module Renderer exposing (Vertex, entity)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import Camera


type alias Vertex =
    { color : Vec3
    , pos : Vec3
    , normal : Vec3
    }


type alias Uniforms =
    { viewing : Mat4
    , perspective : Mat4
    , cameraPos : Vec3
    , lightPos : Vec3
    }


type alias Varyings =
    { vcolor : Vec3
    , vpos : Vec3
    , vnormal : Vec3
    }


entity : Mesh Vertex -> Camera.Model -> WebGL.Entity
entity mesh model =
    let
        uniforms =
            { viewing = Camera.viewingMatrix model
            , perspective = Camera.perspectiveMatrix model
            , cameraPos = vec3 0 0 -Camera.cameraDistance
            , lightPos = vec3 1 1 -2 |> Vec3.scale 5
            }
    in
        WebGL.entity vertexShader fragmentShader mesh uniforms


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|

    attribute vec3 color;
    attribute vec3 pos;
    attribute vec3 normal;
    uniform mat4 viewing;
    uniform mat4 perspective;
    varying vec3 vcolor;
    varying vec3 vpos;
    varying vec3 vnormal;

    void main () {
        vcolor = color;
        vpos = (viewing * vec4(pos, 1.0)).xyz;
        vnormal = (viewing * vec4(pos, 1.0)).xyz;
        gl_Position = perspective * viewing * vec4(pos, 1.0);
    }

    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

    precision mediump float;
    uniform vec3 cameraPos;
    uniform vec3 lightPos;
    varying vec3 vcolor;
    varying vec3 vpos;
    varying vec3 vnormal;

    void main () {
        vec3 N = normalize(vnormal);
        vec3 L = normalize(lightPos - vpos);

        // Lambert's cosine law
        float lambertian = max(dot(N, L), 0.0);

        float specular = 0.0;

        if(lambertian > 0.0) {
          vec3 R = reflect(-L, N);   // Reflected light vector
          vec3 V = normalize(cameraPos - vpos); // Vector to viewer

          // Compute the specular term
          float specAngle = max(dot(R, V), 0.0);
          specular = pow(specAngle, 4.0);
        }
        gl_FragColor = vec4(lambertian * vcolor + 0.2 * specular, 1.0);
    }

    |]
