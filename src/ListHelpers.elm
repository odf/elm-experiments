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
                    List.reverse output

                a :: rest ->
                    if Set.member a seen then
                        step seen rest output
                    else
                        step (Set.insert a seen) rest (a :: output)
    in
        step (Set.empty) aList []


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
    splitWhen (\a -> not (pred a)) aList
        |> (\( lead, trail ) -> (trail ++ lead))
        |> List.filter pred
