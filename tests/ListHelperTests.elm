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
            let
                m =
                    n % max 1 (List.length list)
            in
                ListHelpers.insertAt m a list
                    |> List.take m
                    |> Expect.equalLists (List.take m list)
    , fuzz3 int int (list int) "preserves the later elements" <|
        \n a list ->
            let
                m =
                    n % max 1 (List.length list)
            in
                ListHelpers.insertAt m a list
                    |> List.drop (m + 1)
                    |> Expect.equalLists (List.drop m list)
    , fuzz3 int int (list int) "puts the element at the position" <|
        \n a list ->
            let
                m =
                    n % max 1 (List.length list)
            in
                ListHelpers.insertAt m a list
                    |> List.drop m
                    |> List.head
                    |> Expect.equal (Just a)
    ]


testsForCycle : List Test
testsForCycle =
    [ fuzz2 int (list int) "preserves the length of the list" <|
        \n list ->
            let
                m =
                    n % max 1 (List.length list)
            in
                ListHelpers.cycle m list
                    |> List.length
                    |> Expect.equal (List.length list)
    , fuzz2 int (list int) "puts later elements at the front" <|
        \n list ->
            let
                m =
                    n % max 1 (List.length list)
            in
                ListHelpers.cycle m list
                    |> List.take (List.length list - m)
                    |> Expect.equal (List.drop m list)
    , fuzz2 int (list int) "puts earlier elements at the end" <|
        \n list ->
            let
                m =
                    n % max 1 (List.length list)
            in
                ListHelpers.cycle m list
                    |> List.drop (List.length list - m)
                    |> Expect.equal (List.take m list)
    ]


testsForSortCyclic : List Test
testsForSortCyclic =
    [ fuzz (list int) "preserves the length of the list" <|
        \list ->
            ListHelpers.sortCyclic list
                |> List.length
                |> Expect.equal (List.length list)
    , fuzz (list int) "preserves the elements of the list" <|
        \list ->
            ListHelpers.sortCyclic list
                |> List.sort
                |> Expect.equalLists (List.sort list)
    , fuzz (list int) "is no larger than its cyclic permutations" <|
        \list ->
            let
                sorted =
                    ListHelpers.sortCyclic list
            in
                List.range 0 (List.length sorted - 1)
                    |> List.map (\n -> ListHelpers.cycle n sorted)
                    |> List.all ((<=) sorted)
                    |> Expect.true
                        "should have no smaller cyclic permutation"
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
        , describe "ListHelpers.cycle"
            testsForCycle
        , describe "ListHelpers.sortCyclic"
            testsForSortCyclic
        , describe "ListHelpers.unique"
            testsForUnique
        , describe "ListHelpers.indexWhen"
            testsForIndexWhen
        , describe "ListHelpers.filterCyclicFromSplit"
            testsForFilterCyclicFromSplit
        ]
