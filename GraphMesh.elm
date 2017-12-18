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


dodecahedron : Graph
dodecahedron =
    { adjacencies =
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
    , positions = Nothing
    }


mesh : WebGL.Mesh Renderer.Vertex
mesh =
    WebGL.lines <| lines (embed tutte dodecahedron)



-- Code for Tutte embedder starts here


tailFrom : a -> List a -> List a
tailFrom a aList =
    case aList of
        [] ->
            []

        x :: rest ->
            if x == a then
                a :: rest
            else
                tailFrom a rest


nextCyclic : a -> List a -> Maybe a
nextCyclic a aList =
    case tailFrom a aList of
        [] ->
            Nothing

        a :: [] ->
            List.head aList

        a :: b :: _ ->
            Just b


face : Int -> Int -> Adjacencies -> Maybe (List Int)
face v0 w0 adj =
    let
        step v w rest =
            case Maybe.andThen (nextCyclic w) (Array.get v adj) of
                Nothing ->
                    Nothing

                Just u ->
                    if u == v0 || List.length rest > Array.length adj then
                        Just (w :: rest)
                    else
                        step u v (w :: rest)
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

        alpha =
            pi / 16

        shifted v =
            Vec3.sub (Vec3.scale (sin alpha) v) (vec3 0 0 (cos alpha))

        specs =
            List.map2 (,) outer (List.map shifted (nGon (List.length outer)))
    in
        List.foldl setValue init specs


center : List Vec3 -> Vec3
center points =
    Vec3.normalize (List.foldl Vec3.add (vec3 0 0 0) points)


amap2 : (a -> b -> c) -> Array a -> Array b -> Array c
amap2 fn xs ys =
    Array.fromList (List.map2 fn (Array.toList xs) (Array.toList ys))


tutteStep : Embedding -> Adjacencies -> Embedding
tutteStep pos adj =
    let
        getPos v =
            Maybe.withDefault (vec3 0 0 0) (Array.get v pos)

        moved p q =
            center [ p, center [ p, q ] ]

        newPos ( p, vs ) =
            center (List.map (\v -> moved p (getPos v)) vs)
    in
        Array.map newPos (amap2 (,) pos adj)


tutte : Adjacencies -> Embedding
tutte adj =
    let
        next _ positions =
            tutteStep positions adj
    in
        case firstFace adj of
            Just f ->
                List.foldl
                    next
                    (tutteInitial f adj)
                    (List.range 1 20)

            Nothing ->
                Array.empty
