module GraphMesh exposing (mesh)

import Array exposing (Array)
import Math.Vector3 exposing (vec3, Vec3)
import WebGL
import Renderer exposing (Vertex)


graph : List ( Vec3, List Int )
graph =
    [ ( (vec3 -1 -1 -1), [ 5, 4, 2 ] )
    , ( (vec3 1 -1 -1), [ 6, 1, 3 ] )
    , ( (vec3 1 1 -1), [ 7, 2, 4 ] )
    , ( (vec3 -1 1 -1), [ 8, 3, 1 ] )
    , ( (vec3 -1 -1 1), [ 1, 6, 8 ] )
    , ( (vec3 1 -1 1), [ 2, 7, 5 ] )
    , ( (vec3 1 1 1), [ 3, 8, 6 ] )
    , ( (vec3 -1 1 1), [ 4, 5, 7 ] )
    ]


lines : List ( Vec3, List Int ) -> List ( Vertex, Vertex )
lines items =
    let
        n =
            List.length items

        positions =
            Array.fromList <| List.map (\( v, _ ) -> v) items

        vertex pos =
            { color = (vec3 1 1 1)
            , pos = pos
            , normal = pos
            }

        edge p1 p2 =
            ( (vertex p1), (vertex p2) )

        getPos v =
            case Array.get (v - 1) positions of
                Nothing ->
                    vec3 0 0 0

                Just p ->
                    p

        edges v ( _, adj ) =
            List.map (\w -> edge (getPos v) (getPos w)) adj
    in
        List.concat <| List.map2 edges (List.range 1 n) items


mesh : WebGL.Mesh Renderer.Vertex
mesh =
    WebGL.lines <| lines graph
