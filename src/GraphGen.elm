module GraphGen
    exposing
        ( tetrahedron
        )

import SurfaceGraph exposing (..)


tailFrom : a -> List a -> List a
tailFrom a aList =
    case aList of
        [] ->
            []

        x :: rest ->
            if x == a then
                aList
            else
                tailFrom a rest


nextCyclic : a -> List a -> Maybe a
nextCyclic a aList =
    case tailFrom a aList of
        [] ->
            Nothing

        a :: [] ->
            List.head aList

        a :: b :: _ ->
            Just b


nextAtVertex : Int -> Int -> Graph -> Maybe Int
nextAtVertex v w gr =
    nextCyclic w <| neighbors v gr


nextAtFace : Int -> Int -> Graph -> Maybe Int
nextAtFace v w gr =
    nextCyclic v <| List.reverse <| neighbors w gr


tetrahedron : Graph
tetrahedron =
    graph
        [ [ 1, 2, 3 ], [ 0, 3, 2 ], [ 0, 1, 3 ], [ 0, 2, 1 ] ]


add3Vertex : Int -> Int -> Graph -> Graph
add3Vertex v w gr =
    case nextAtVertex v w gr of
        Nothing ->
            gr

        Just u ->
            addVertex [ u, v, w ] gr


dummy : Graph
dummy =
    Debug.log "test" <| add3Vertex 0 1 tetrahedron
