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


expectMatchingFirsts :
    Set comparable
    -> List comparable
    -> List comparable
    -> Expectation
expectMatchingFirsts seen listA listB =
    let
        seenFirst list =
            mapHead (\x -> Set.member x seen) False list

        rememberFirst list =
            mapHead (\x -> Set.insert x seen) seen list
    in
        if List.isEmpty listA && List.isEmpty listB then
            Expect.pass
        else if seenFirst listA then
            expectMatchingFirsts seen (List.drop 1 listA) listB
        else if seenFirst listB then
            expectMatchingFirsts seen listA (List.drop 1 listB)
        else if List.head listA == List.head listB then
            expectMatchingFirsts
                (rememberFirst listA)
                (List.drop 1 listA)
                (List.drop 1 listB)
        else
            Expect.fail "expected order of first appearances to match"


pred : Int -> Bool
pred n =
    n % 3 == 0


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
                    ListHelpers.unique list
                        |> List.sort
                        |> (\xs -> List.map2 (,) xs (List.drop 1 xs))
                        |> List.filter (\( a, b ) -> a == b)
                        |> Expect.equalLists []
            , fuzz (list int) "preserves order" <|
                \list ->
                    expectMatchingFirsts
                        Set.empty
                        list
                        (ListHelpers.unique list)
            ]
        , describe "ListHelper.splitWhen"
            [ fuzz (list int) "sublists rejoin to original list" <|
                \list ->
                    ListHelpers.splitWhen pred list
                        |> (\( lead, trail ) -> (lead ++ trail))
                        |> Expect.equalLists list
            , fuzz (list int) "first sublist members fail predicate" <|
                \list ->
                    ListHelpers.splitWhen pred list
                        |> (\( lead, trail ) -> lead)
                        |> List.filter pred
                        |> Expect.equalLists []
            , fuzz (list int) "second sublist head passes predicate" <|
                \list ->
                    ListHelpers.splitWhen pred list
                        |> (\( lead, trail ) -> trail)
                        |> mapHead pred True
                        |> Expect.true "expected to pass predicate"
            ]
        , describe "ListHelper.filterCyclicFromSplit"
            [ fuzz (list int) "filters while preserving cyclic runs" <|
                \list ->
                    ListHelpers.splitWhen (\a -> not (pred a)) list
                        |> (\( lead, trail ) -> (trail ++ lead))
                        |> List.filter pred
                        |> Expect.equalLists
                            (ListHelpers.filterCyclicFromSplit
                                pred
                                list
                            )
            ]
        ]
