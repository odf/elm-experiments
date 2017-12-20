module GraphMesh exposing (mesh)

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL
import Embed
import Renderer exposing (Vertex)


type alias Graph =
    { adjacencies : Embed.Adjacencies
    , positions : Maybe Embed.Embedding
    }


makeGraph : List (List Int) -> Graph
makeGraph adj =
    { adjacencies = Array.fromList adj
    , positions = Nothing
    }


vertex : Vec3 -> Renderer.Vertex
vertex pos =
    { color = (vec3 1 1 1)
    , pos = pos
    , normal = pos
    }


lines : Graph -> List ( Vertex, Vertex )
lines graph =
    let
        positions =
            Maybe.withDefault Array.empty graph.positions

        edge p1 p2 =
            ( (vertex p1), (vertex p2) )

        getPos v =
            Array.get v positions

        edges ( v, adj ) =
            List.filterMap (\w -> Maybe.map2 edge (getPos v) (getPos w)) adj
    in
        List.concat <|
            List.map edges (Array.toIndexedList graph.adjacencies)


embed : Embed.Embedder -> Graph -> Graph
embed embedder graph =
    { graph | positions = Just (embedder graph.adjacencies) }



-- Some example graphs


cube : Graph
cube =
    makeGraph
        [ [ 4, 3, 1 ]
        , [ 5, 0, 2 ]
        , [ 6, 1, 3 ]
        , [ 7, 2, 0 ]
        , [ 0, 5, 7 ]
        , [ 1, 6, 4 ]
        , [ 2, 7, 5 ]
        , [ 3, 4, 6 ]
        ]


dodecahedron : Graph
dodecahedron =
    makeGraph
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



-- Using a fixed example graph for now


mesh : WebGL.Mesh Renderer.Vertex
mesh =
    WebGL.lines <| lines (embed Embed.default dodecahedron)
