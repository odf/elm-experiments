module GraphMesh exposing (mesh)

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
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
    WebGL.lines <| lines (embed tutte cube)



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


face : Int -> Int -> Adjacencies -> Maybe (List Int)
face v0 w0 adj =
    let
        next u v vs =
            if u == v0 then
                Just (u :: vs)
            else
                step u v (u :: vs)

        step v w vs =
            Array.get v adj
                |> Maybe.andThen (\nbs -> nextCyclic w nbs)
                |> Maybe.andThen (\u -> next u v vs)
    in
        step v0 w0 []


firstFace : Adjacencies -> Maybe (List Int)
firstFace adj =
    Array.get 0 adj
        |> Maybe.andThen (\vs -> List.head vs)
        |> Maybe.andThen (\w -> face 0 w adj)


nGon : Int -> List Vec3
nGon n =
    let
        angle i =
            2 * pi * (toFloat i) / (toFloat n)

        corner i =
            vec3 (cos (angle i)) (sin (angle i)) 0.0
    in
        List.map corner (List.range 0 (n - 1))


tutteInitial : List Int -> Adjacencies -> Embedding
tutteInitial outer adj =
    let
        setValue ( idx, val ) a =
            Array.set idx val a

        init =
            (Array.initialize (Array.length adj) (\_ -> vec3 0 0 1))

        shifted v =
            Vec3.sub (Vec3.scale (cos (pi / 3)) v) (vec3 0 0 (sin (pi / 3)))

        specs =
            List.map2 (,) outer (List.map shifted (nGon (List.length outer)))
    in
        List.foldl setValue init specs


tutte : Adjacencies -> Embedding
tutte adj =
    case firstFace adj of
        Just f ->
            tutteInitial f adj

        Nothing ->
            Array.empty
