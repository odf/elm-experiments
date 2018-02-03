module GraphGenTests exposing (suite)

import Expect
import Test exposing (..)
import SurfaceGraph
import GraphGen


adjacencies : SurfaceGraph.Graph -> List (List Int)
adjacencies gr =
    List.range 0 (SurfaceGraph.nrVertices gr - 1)
        |> List.map (\v -> SurfaceGraph.neighbors v gr)


suite : Test
suite =
    describe "The GraphGen module"
        [ test "An explicit sequence of graph operations" <|
            \_ ->
                SurfaceGraph.tetrahedron
                    |> GraphGen.addNVertex 3 0 1
                    |> GraphGen.addNVertex 5 0 1
                    |> SurfaceGraph.removeEdge 0 1
                    |> adjacencies
                    |> Expect.equal
                        [ [ 3, 5 ]
                        , [ 2, 4, 5, 3 ]
                        , [ 1, 3, 5, 4 ]
                        , [ 0, 5, 2, 1 ]
                        , [ 1, 2, 5 ]
                        , [ 0, 1, 4, 2, 3 ]
                        ]
        , test "An integer-encoded sequence of graph operations" <|
            \_ ->
                SurfaceGraph.tetrahedron
                    |> GraphGen.grow 24 16
                    |> GraphGen.grow 36 26
                    |> GraphGen.shrink 49
                    |> adjacencies
                    |> Expect.equal
                        [ [ 1, 5, 3 ]
                        , [ 0, 3, 4, 5 ]
                        , [ 3, 5, 4 ]
                        , [ 0, 5, 2, 1 ]
                        , [ 1, 2, 5 ]
                        , [ 0, 1, 4, 2, 3 ]
                        ]
        , test "A compactly encoded sequence of graph operations" <|
            \_ ->
                GraphGen.build [ ( 24, 16 ), ( 36, 26 ) ] [ 49 ]
                    |> adjacencies
                    |> Expect.equal
                        [ [ 1, 5, 3 ]
                        , [ 0, 3, 4, 5 ]
                        , [ 3, 5, 4 ]
                        , [ 0, 5, 2, 1 ]
                        , [ 1, 2, 5 ]
                        , [ 0, 1, 4, 2, 3 ]
                        ]
        ]
