module GraphGen
    exposing
        ( tetrahedron
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
            let
                inner =
                    vs |> List.drop 1 |> List.take (n - 3)
            in
                List.foldl (\u -> removeEdge v u) gr inner
                    |> addVertex ([ v ] ++ vs)


dummy : Graph
dummy =
    tetrahedron
        |> addNVertex 3 0 1
        |> addNVertex 5 0 1
        |> removeEdge 0 1
        |> Debug.log "test"
