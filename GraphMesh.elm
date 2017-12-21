module GraphMesh exposing (mesh, cube, dodecahedron)

import Math.Vector3 exposing (vec3)
import WebGL
import Embed
import Renderer


mesh : Embed.Embedder -> Embed.Adjacencies -> WebGL.Mesh Renderer.Vertex
mesh embedder adj =
    let
        pos =
            embedder adj

        meshVertex v =
            let
                p =
                    Embed.getPos v pos
            in
                { color = (vec3 1 1 1), pos = p, normal = p }

        meshEdge ( v, w ) =
            ( meshVertex v, meshVertex w )
    in
        WebGL.lines <| List.map meshEdge <| Embed.edges adj



-- Some example graphs


cube : Embed.Adjacencies
cube =
    Embed.adjacencies
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
    Embed.adjacencies
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
