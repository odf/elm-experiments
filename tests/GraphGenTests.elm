module GraphGenTests exposing (suite)

import Expect
import Test exposing (..)
import SurfaceGraph
import GraphGen


suite : Test
suite =
    describe "The GraphGen module"
        [ test "An example sequence of graph operations" <|
            \_ ->
                GraphGen.tetrahedron
                    |> GraphGen.addNVertex 3 0 1
                    |> GraphGen.addNVertex 5 0 1
                    |> SurfaceGraph.removeEdge 0 1
                    |> Expect.equal
                        (SurfaceGraph.graph
                            [ [ 3, 5 ]
                            , [ 2, 4, 5, 3 ]
                            , [ 1, 3, 5, 4 ]
                            , [ 0, 5, 2, 1 ]
                            , [ 1, 2, 5 ]
                            , [ 0, 1, 4, 2, 3 ]
                            ]
                        )
        , test "Another example sequence of graph operations" <|
            \_ ->
                GraphGen.tetrahedron
                    |> GraphGen.grow 24 16
                    |> GraphGen.grow 36 26
                    |> GraphGen.shrink 49
                    |> Expect.equal
                        (SurfaceGraph.graph
                            [ [ 1, 5, 3 ]
                            , [ 0, 3, 4, 5 ]
                            , [ 3, 5, 4 ]
                            , [ 0, 5, 2, 1 ]
                            , [ 1, 2, 5 ]
                            , [ 0, 1, 4, 2, 3 ]
                            ]
                        )
        ]
