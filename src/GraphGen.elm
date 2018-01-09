module GraphGen
    exposing
        ( tetrahedron
        , addNVertex
        , grow
        )

import ListHelpers
import SurfaceGraph exposing (..)


tetrahedron : Graph
tetrahedron =
    graph
        [ [ 1, 2, 3 ], [ 0, 3, 2 ], [ 0, 1, 3 ], [ 0, 2, 1 ] ]


neighborsFrom : Int -> Int -> Int -> Graph -> Maybe (List Int)
neighborsFrom m v w gr =
    let
        nbs =
            neighbors v gr

        grabFrom i =
            List.take m (ListHelpers.cycle i nbs)
    in
        if List.length nbs < m then
            Nothing
        else
            ListHelpers.indexWhen ((==) w) nbs |> Maybe.map grabFrom


addNVertex : Int -> Int -> Int -> Graph -> Graph
addNVertex n v w gr =
    case neighborsFrom (n - 1) v w gr of
        Nothing ->
            gr

        Just vs ->
            List.take (n - 3) (List.drop 1 vs)
                |> List.foldl (\u -> removeEdge v u) gr
                |> addVertex ([ v ] ++ vs)


pick : Int -> List a -> Maybe a
pick i es =
    List.drop (i % List.length es) es |> List.head


grow : Int -> Int -> Graph -> Graph
grow a b gr =
    let
        ( v, w ) =
            directedEdges gr |> pick a |> Maybe.withDefault ( 0, 0 )

        n =
            b % (min (degree v gr - 1) 3) + 3
    in
        addNVertex n v w gr
