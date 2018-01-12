module SurfaceGraphTests exposing (suite)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import Array exposing (Array)
import SurfaceGraph as Graph exposing (Graph)
import GraphGen


shortList : Fuzzer a -> Fuzzer (List a)
shortList fzz =
    Fuzz.conditional
        { condition = List.length >> (>=) 20
        , retries = 1
        , fallback = List.take 20
        }
        (list fzz)


graph : Fuzzer Graph
graph =
    Fuzz.map2
        GraphGen.build
        (shortList <| tuple ( int, int ))
        (shortList int)


generalTests : List Test
generalTests =
    [ fuzz graph "graph is reconstructed from neighbor lists" <|
        \gr ->
            List.range 0 (Graph.nrVertices gr - 1)
                |> List.map (\v -> Graph.neighbors v gr)
                |> Graph.graph
                |> Expect.equal gr
    , fuzz graph "directed edges come in opposite pairs" <|
        \gr ->
            let
                es =
                    Graph.directedEdges gr |> List.sort
            in
                List.map (\( v, w ) -> ( w, v )) es
                    |> List.sort
                    |> Expect.equal es
    , fuzz graph "sum of vertex degrees is twice the edge count" <|
        \gr ->
            List.range 0 (Graph.nrVertices gr - 1)
                |> List.map (\v -> Graph.degree v gr)
                |> List.sum
                |> Expect.equal (2 * List.length (Graph.edges gr))
    ]


distanceMap : Int -> Graph -> Array Int
distanceMap start gr =
    let
        update ( i, vs ) arr =
            List.foldl (\v -> Array.set v i) arr vs
    in
        Graph.verticesByDistance start gr
            |> List.indexedMap (,)
            |> List.foldl update (Array.repeat (Graph.nrVertices gr) 0)


testsForVerticesByDistance : List Test
testsForVerticesByDistance =
    [ fuzz2 graph int "only the start vertex is in the distance 0 layer" <|
        \gr n ->
            let
                start =
                    n % Graph.nrVertices gr
            in
                Graph.verticesByDistance start gr
                    |> List.head
                    |> Expect.equal (Just [ start ])
    , fuzz2 graph int "each vertex is in exactly one layer" <|
        \gr n ->
            Graph.verticesByDistance (n % Graph.nrVertices gr) gr
                |> List.concat
                |> List.sort
                |> Expect.equal (List.range 0 (Graph.nrVertices gr - 1))
    , fuzz2 graph int "adjacent edges should be in equal or adjacent layers" <|
        \gr n ->
            let
                dists =
                    distanceMap (n % Graph.nrVertices gr) gr

                good ( v, w ) =
                    Maybe.map2
                        (\a b -> abs (a - b) <= 1)
                        (Array.get v dists)
                        (Array.get w dists)
                        |> (==) (Just True)
            in
                Graph.edges gr
                    |> List.all good
                    |> Expect.true
                        "depth difference along edge should be at most 1"
    , fuzz2 graph int "each vertex at a positive level has a lower neighbor" <|
        \gr n ->
            let
                start =
                    n % Graph.nrVertices gr

                dists =
                    distanceMap start gr

                hasLowerNeighbor v =
                    let
                        d0 =
                            Array.get v dists |> Maybe.withDefault 0
                    in
                        Graph.neighbors v gr
                            |> List.filterMap (\w -> Array.get w dists)
                            |> List.any (\d -> d < d0)
            in
                List.range 0 (Graph.nrVertices gr - 1)
                    |> List.filter ((/=) start)
                    |> List.all hasLowerNeighbor
                    |> Expect.true
                        "each vertex except start must have a lower neighbor"
    ]


suite : Test
suite =
    describe "The SurfaceGraph module"
        [ describe "general tests" generalTests
        , describe "SurfaceGraph.verticesByDistance"
            testsForVerticesByDistance
        ]
