module GraphGen
    exposing
        ( tetrahedron
        , addNVertex
        , grow
        , shrink
        , build
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
                |> triangulateFaceFromCenter v w


grow : Int -> Int -> Graph -> Graph
grow a b gr =
    case
        directedEdges gr
            |> ListHelpers.pickCyclic a
    of
        Nothing ->
            gr

        Just ( v, w ) ->
            let
                k =
                    min (degree v gr - 1) 3
            in
                addNVertex (b % k + 3) v w gr


removableEdge : Int -> Int -> Graph -> Bool
removableEdge v w gr =
    (degree v gr > 3)
        && (degree w gr > 3)
        && (ListHelpers.intersectLists (face v w gr) (face w v gr)
                |> List.length
                |> (==) 2
           )


shrink : Int -> Graph -> Graph
shrink a gr =
    case
        directedEdges gr
            |> List.filter (\( v, w ) -> removableEdge v w gr)
            |> ListHelpers.pickCyclic a
    of
        Nothing ->
            gr

        Just ( v, w ) ->
            removeEdge v w gr


build : List ( Int, Int ) -> List Int -> Graph
build growParms shrinkParms =
    tetrahedron
        |> (\gr -> List.foldl (\( a, b ) -> grow a b) gr growParms)
        |> (\gr -> List.foldl shrink gr shrinkParms)
