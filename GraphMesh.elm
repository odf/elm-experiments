module GraphMesh exposing (mesh, cube, dodecahedron)

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL
import Embed
import Renderer exposing (Vertex)


vertex : Vec3 -> Renderer.Vertex
vertex pos =
    { color = (vec3 1 1 1)
    , pos = pos
    , normal = pos
    }


lines : Embed.Embedder -> Embed.Adjacencies -> List ( Vertex, Vertex )
lines embedder adj =
    let
        pos =
            embedder adj

        makeVertex v =
            vertex <| Maybe.withDefault (vec3 0 0 0) <| Array.get v pos

        edges ( v, adj ) =
            List.map (\w -> ( (makeVertex v), (makeVertex w) )) adj
    in
        List.concat <| List.map edges <| Array.toIndexedList adj


mesh : Embed.Embedder -> Embed.Adjacencies -> WebGL.Mesh Renderer.Vertex
mesh embedder =
    WebGL.lines << lines embedder



-- Some example graphs


cube : Embed.Adjacencies
cube =
    Array.fromList
        [ [ 4, 3, 1 ]
        , [ 5, 0, 2 ]
        , [ 6, 1, 3 ]
        , [ 7, 2, 0 ]
        , [ 0, 5, 7 ]
        , [ 1, 6, 4 ]
        , [ 2, 7, 5 ]
        , [ 3, 4, 6 ]
        ]


dodecahedron : Embed.Adjacencies
dodecahedron =
    Array.fromList
        [ [ 1, 5, 4 ]
        , [ 2, 6, 0 ]
        , [ 3, 7, 1 ]
        , [ 4, 8, 2 ]
        , [ 0, 9, 3 ]
        , [ 0, 11, 10 ]
        , [ 1, 12, 11 ]
        , [ 2, 13, 12 ]
        , [ 3, 14, 13 ]
        , [ 4, 10, 14 ]
        , [ 5, 15, 9 ]
        , [ 6, 19, 5 ]
        , [ 7, 18, 6 ]
        , [ 8, 17, 7 ]
        , [ 9, 16, 8 ]
        , [ 10, 19, 16 ]
        , [ 14, 15, 17 ]
        , [ 13, 16, 18 ]
        , [ 12, 17, 19 ]
        , [ 11, 18, 15 ]
        ]
