module ListHelpers
    exposing
        ( unique
        , indexWhen
        , filterCyclicFromSplit
        , cycle
        , insertBefore
        )

import Set exposing (Set)


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
    indexWhen (\a -> not (pred a)) aList
        |> Maybe.withDefault (List.length aList)
        |> (\n -> (List.drop n aList) ++ (List.take n aList))
        |> List.filter pred


insertBefore : a -> a -> List a -> List a
insertBefore a b aList =
    indexWhen ((==) a) aList
        |> Maybe.withDefault 0
        |> (\n -> (List.take n aList) ++ [ b ] ++ (List.drop n aList))


cycle : Int -> List a -> List a
cycle n aList =
    (List.drop n aList) ++ (List.take n aList)
