module ListHelperTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Set exposing (Set)
import ListHelpers


mapHead : (a -> b) -> b -> List a -> b
mapHead fn default list =
    case list of
        [] ->
            default

        x :: xs ->
            fn x


firstAppearanceOrderMatches :
    Set comparable
    -> List comparable
    -> List comparable
    -> Expectation
firstAppearanceOrderMatches seen listA listB =
    let
        seenFirst list =
            mapHead (\x -> Set.member x seen) False list

        rememberFirst list =
            mapHead (\x -> Set.insert x seen) seen list
    in
        if List.isEmpty listA && List.isEmpty listB then
            Expect.pass
        else if seenFirst listA then
            firstAppearanceOrderMatches seen (List.drop 1 listA) listB
        else if seenFirst listB then
            firstAppearanceOrderMatches seen listA (List.drop 1 listB)
        else if List.head listA == List.head listB then
            firstAppearanceOrderMatches
                (rememberFirst listA)
                (List.drop 1 listA)
                (List.drop 1 listB)
        else
            Expect.fail "expected order of first appearances to match"


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
            , fuzz (list int) "preserves order" <|
                \list ->
                    firstAppearanceOrderMatches
                        Set.empty
                        list
                        (ListHelpers.unique list)
            ]
        ]
