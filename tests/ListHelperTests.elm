module ListHelperTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (int, list)
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


testsForInsertAt : List Test
testsForInsertAt =
    [ fuzz3 int int (list int) "preserves the earlier elements" <|
        \n a list ->
            if n <= List.length list then
                Expect.equalLists
                    (List.take n list)
                    (List.take n (ListHelpers.insertAt n a list))
            else
                Expect.pass
    , fuzz3 int int (list int) "preserves the later elements" <|
        \n a list ->
            if 0 <= n then
                Expect.equalLists
                    (List.drop n list)
                    (List.drop (n + 1) (ListHelpers.insertAt n a list))
            else
                Expect.pass
    , fuzz3 int int (list int) "puts the element at the position" <|
        \n a list ->
            if 0 <= n && n <= List.length list then
                ListHelpers.insertAt n a list
                    |> List.drop n
                    |> List.head
                    |> Expect.equal (Just a)
            else
                Expect.pass
    ]


testsForUnique : List Test
testsForUnique =
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
            ListHelpers.unique list
                |> expectMatchingFirsts Set.empty list
    ]


testsForIndexWhen : List Test
testsForIndexWhen =
    [ fuzz (list int) "earlier list members fail predicate" <|
        \list ->
            ListHelpers.indexWhen pred list
                |> Maybe.withDefault (List.length list)
                |> (\n -> List.take n list)
                |> List.filter pred
                |> Expect.equalLists []
    , fuzz (list int) "list at determined index passes predicate" <|
        \list ->
            ListHelpers.indexWhen pred list
                |> Maybe.withDefault (List.length list)
                |> (\n -> List.drop n list)
                |> mapHead pred True
                |> Expect.true "expected to pass predicate"
    ]


testsForFilterCyclicFromSplit : List Test
testsForFilterCyclicFromSplit =
    [ fuzz (list int) "filters while preserving cyclic runs" <|
        \list ->
            ListHelpers.indexWhen (\a -> not (pred a)) list
                |> Maybe.withDefault (List.length list)
                |> (\n -> (List.drop n list) ++ (List.take n list))
                |> List.filter pred
                |> Expect.equalLists
                    (ListHelpers.filterCyclicFromSplit pred list)
    ]


suite : Test
suite =
    describe "The ListHelpers module"
        [ describe "ListHelpers.insertAt"
            testsForInsertAt
        , describe "ListHelpers.unique"
            testsForUnique
        , describe "ListHelpers.indexWhen"
            testsForIndexWhen
        , describe "ListHelpers.filterCyclicFromSplit"
            testsForFilterCyclicFromSplit
        ]
