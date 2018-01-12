module ListHelpers
    exposing
        ( insertAt
        , cycle
        , sortCyclic
        , unique
        , indexWhen
        , filterCyclicFromSplit
        , pickCyclic
        )

import Set exposing (Set)


insertAt : Int -> a -> List a -> List a
insertAt n a aList =
    (List.take n aList) ++ [ a ] ++ (List.drop n aList)


cycle : Int -> List a -> List a
cycle n aList =
    (List.drop n aList) ++ (List.take n aList)


sortCyclic : List comparable -> List comparable
sortCyclic aList =
    List.range 0 (List.length aList - 1)
        |> List.map (\n -> cycle n aList)
        |> List.minimum
        |> Maybe.withDefault aList


unique : List comparable -> List comparable
unique aList =
    let
        step seen remaining output =
            case remaining of
                [] ->
                    List.reverse output

                a :: rest ->
                    if Set.member a seen then
                        step seen rest output
                    else
                        step (Set.insert a seen) rest (a :: output)
    in
        step (Set.empty) aList []


indexWhen : (a -> Bool) -> List a -> Maybe Int
indexWhen pred aList =
    let
        step n remaining =
            case remaining of
                [] ->
                    Nothing

                a :: rest ->
                    if pred a then
                        Just n
                    else
                        step (n + 1) rest
    in
        step 0 aList


filterCyclicFromSplit : (a -> Bool) -> List a -> List a
filterCyclicFromSplit pred aList =
    indexWhen (not << pred) aList
        |> Maybe.withDefault 0
        |> (\n -> cycle n aList)
        |> List.filter pred


pickCyclic : Int -> List a -> Maybe a
pickCyclic i es =
    List.drop (i % max 1 (List.length es)) es |> List.head
