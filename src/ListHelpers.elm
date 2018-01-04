module ListHelpers
    exposing
        ( unique
        , splitWhen
        , filterCyclicFromSplit
        )

import Set exposing (Set)


unique : List comparable -> List comparable
unique aList =
    let
        step seen remaining output =
            case remaining of
                [] ->
                    output

                a :: rest ->
                    if Set.member a seen then
                        step seen rest output
                    else
                        step (Set.insert a seen) rest (a :: output)
    in
        List.reverse <| step (Set.empty) aList []


splitWhen : (a -> Bool) -> List a -> ( List a, List a )
splitWhen pred aList =
    let
        step leading remaining =
            case remaining of
                [] ->
                    ( List.reverse leading, [] )

                a :: rest ->
                    if pred a then
                        ( List.reverse leading, remaining )
                    else
                        step (a :: leading) rest
    in
        step [] aList


filterCyclicFromSplit : (a -> Bool) -> List a -> List a
filterCyclicFromSplit pred aList =
    let
        ( leading, trailing ) =
            splitWhen (\a -> not (pred a)) aList
    in
        List.filter pred (trailing ++ leading)
