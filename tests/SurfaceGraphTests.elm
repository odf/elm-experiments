module SurfaceGraphTests exposing (suite)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import Array exposing (Array)
import ListHelpers
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


graphWithEdge : Fuzzer ( Graph, Int, Int )
graphWithEdge =
    Fuzz.map2
        (\gr a ->
            Graph.directedEdges gr
                |> ListHelpers.pickCyclic a
                |> Maybe.withDefault ( 0, 0 )
                |> (\( v, w ) -> ( gr, v, w ))
        )
        graph
        int


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
    , fuzz2 graph int "each positive level vertex has a neighbor one lower" <|
        \gr n ->
            let
                start =
                    n % Graph.nrVertices gr

                dists =
                    distanceMap start gr

                hasNeighborOneLower v =
                    let
                        d0 =
                            Array.get v dists |> Maybe.withDefault 0
                    in
                        Graph.neighbors v gr
                            |> List.filterMap (\w -> Array.get w dists)
                            |> List.any ((==) (d0 - 1))
            in
                List.range 0 (Graph.nrVertices gr - 1)
                    |> List.filter ((/=) start)
                    |> List.all hasNeighborOneLower
                    |> Expect.true
                        "each vertex not start must have a neighbor one lower"
    ]


testsForFaces : List Test
testsForFaces =
    [ fuzz graph "directed edge sets by vertex and by face are the same" <|
        \gr ->
            Graph.faces gr
                |> List.map ListHelpers.cyclicPairs
                |> List.concat
                |> List.sort
                |> Expect.equal (List.sort (Graph.directedEdges gr))
    ]


normalized =
    List.map ListHelpers.sortCyclic >> List.sort


testsForTriangulate : List Test
testsForTriangulate =
    [ fuzz graphWithEdge "the modified graph has the expected faces" <|
        \( gr, v, w ) ->
            let
                n =
                    Graph.nrVertices gr

                facesOld =
                    Graph.faces gr

                facesNew =
                    Graph.triangulateFaceFromCenter v w gr |> Graph.faces

                deleted =
                    [ Graph.face v w gr ]

                added =
                    Graph.face v w gr
                        |> ListHelpers.cyclicPairs
                        |> List.map (\( v, w ) -> [ v, w, n ])
            in
                Expect.equalLists
                    ((facesOld ++ added) |> normalized)
                    ((facesNew ++ deleted) |> normalized)
    ]


faceTail : Int -> Int -> Graph -> List Int
faceTail v w gr =
    let
        f =
            Graph.face v w gr
    in
        ListHelpers.indexWhen ((==) w) f
            |> Maybe.withDefault 0
            |> (\n -> ListHelpers.cycle n f)
            |> List.drop 1


testsForRemoveEdge : List Test
testsForRemoveEdge =
    [ fuzz graphWithEdge "the modified graph has the expected faces" <|
        \( gr, v, w ) ->
            if Graph.removableEdge v w gr then
                let
                    facesOld =
                        Graph.faces gr

                    facesNew =
                        Graph.removeEdge v w gr |> Graph.faces

                    deleted =
                        [ Graph.face v w gr, Graph.face w v gr ]

                    added =
                        [ (faceTail v w gr) ++ (faceTail w v gr) ]
                in
                    Expect.equalLists
                        ((facesOld ++ added) |> normalized)
                        ((facesNew ++ deleted) |> normalized)
            else
                Expect.pass
    ]


suite : Test
suite =
    describe "The SurfaceGraph module"
        [ describe "general tests"
            generalTests
        , describe "SurfaceGraph.verticesByDistance"
            testsForVerticesByDistance
        , describe "SurfaceGraph.faces"
            testsForFaces
        , describe "SurfaceGraph.triangulateFaceFromCenter"
            testsForTriangulate
        , describe "SurfaceGraph.removeEdge"
            testsForRemoveEdge
        ]
