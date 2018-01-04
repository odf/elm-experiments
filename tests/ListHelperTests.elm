module ListHelperTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Set exposing (Set)
import ListHelpers


suite : Test
suite =
    describe "The ListHelper module"
        [ describe "ListHelper.unique"
            [ fuzz (list int) "preserves the set of elements" <|
                \list ->
                    Expect.equalSets
                        (Set.fromList list)
                        (Set.fromList <| ListHelpers.unique list)
            , fuzz (list int) "removes duplicates" <|
                \list ->
                    let
                        squashed =
                            List.sort <| ListHelpers.unique list
                    in
                        List.map2 (,) squashed (List.drop 1 squashed)
                            |> List.filter (\( a, b ) -> a == b)
                            |> List.length
                            |> Expect.equal 0
            ]
        ]
