module GraphMesh exposing (mesh)

import Array exposing (Array)
import Math.Vector3 exposing (vec3, Vec3)
import WebGL
import Renderer exposing (Vertex)


type alias Graph =
    { adjacencies : List (List Int)
    , positions : Maybe (List Vec3)
    }


cube : Graph
cube =
    { adjacencies =
        [ [ 5, 4, 2 ]
        , [ 6, 1, 3 ]
        , [ 7, 2, 4 ]
        , [ 8, 3, 1 ]
        , [ 1, 6, 8 ]
        , [ 2, 7, 5 ]
        , [ 3, 8, 6 ]
        , [ 4, 5, 7 ]
        ]
    , positions =
        Just
            [ (vec3 -1 -1 -1)
            , (vec3 1 -1 -1)
            , (vec3 1 1 -1)
            , (vec3 -1 1 -1)
            , (vec3 -1 -1 1)
            , (vec3 1 -1 1)
            , (vec3 1 1 1)
            , (vec3 -1 1 1)
            ]
    }


vertex : Vec3 -> Renderer.Vertex
vertex pos =
    { color = (vec3 1 1 1)
    , pos = pos
    , normal = pos
    }


positions : Graph -> List Vec3
positions graph =
    Maybe.withDefault [] graph.positions


lines : Graph -> List ( Vertex, Vertex )
lines graph =
    let
        n =
            List.length graph.adjacencies

        ps =
            Array.fromList (positions graph)

        edge p1 p2 =
            ( (vertex p1), (vertex p2) )

        getPos v =
            Array.get (v - 1) ps

        edges v adj =
            List.filterMap (\w -> Maybe.map2 edge (getPos v) (getPos w)) adj
    in
        List.concat <| List.map2 edges (List.range 1 n) graph.adjacencies


mesh : WebGL.Mesh Renderer.Vertex
mesh =
    WebGL.lines <| lines cube
