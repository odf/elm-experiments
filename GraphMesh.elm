module GraphMesh exposing (mesh)

import Array exposing (Array)
import Math.Vector3 exposing (vec3, Vec3)
import WebGL
import Renderer exposing (Vertex)


type alias Adjacencies =
    Array (List Int)


type alias Embedding =
    Array Vec3


type alias Graph =
    { adjacencies : Adjacencies
    , positions : Maybe Embedding
    }


type alias Embedder =
    Adjacencies -> Embedding


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


embed : Embedder -> Graph -> Graph
embed embedder graph =
    { graph | positions = Just (embedder graph.adjacencies) }



-- Using a fixed example graph for now


cube : Graph
cube =
    { adjacencies =
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
    , positions =
        Just <|
            Array.fromList
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


mesh : WebGL.Mesh Renderer.Vertex
mesh =
    WebGL.lines <| lines cube



-- Code for Tutte embedder starts here


nextCyclic : Int -> List Int -> Maybe Int
nextCyclic v vs =
    case vs of
        v :: w :: rest ->
            Just w

        v :: [] ->
            List.head vs

        [] ->
            Nothing


prevEdge : ( Int, Int ) -> Adjacencies -> Maybe ( Int, Int )
prevEdge ( v, w ) adjacencies =
    let
        nbs =
            Maybe.withDefault [] (Array.get v adjacencies)
    in
        Maybe.andThen (\u -> Just ( u, v )) (nextCyclic w nbs)
