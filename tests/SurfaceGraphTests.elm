module SurfaceGraphTests exposing (suite)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
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


suite : Test
suite =
    describe "The SurfaceGraph module"
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
