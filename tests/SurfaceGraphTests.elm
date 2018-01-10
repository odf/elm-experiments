module SurfaceGraphTests exposing (suite)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import SurfaceGraph exposing (Graph)
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
        [ fuzz graph "testing random graph generation only" <|
            \_ -> Expect.pass
        ]
