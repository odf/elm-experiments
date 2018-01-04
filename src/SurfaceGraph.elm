module SurfaceGraph
    exposing
        ( Graph
        , graph
        , neighbors
        , nrVertices
        , edges
        , verticesByDistance
        )

import Array exposing (Array)
import Set exposing (Set)
import ListHelpers


type Graph
    = Graph (Array (List Int))


graph : List (List Int) -> Graph
graph =
    Graph << Array.fromList


neighbors : Int -> Graph -> List Int
neighbors v (Graph adj) =
    Maybe.withDefault [] (Array.get v adj)


nrVertices : Graph -> Int
nrVertices (Graph adj) =
    Array.length adj


edges : Graph -> List ( Int, Int )
edges (Graph adj) =
    let
        incident ( v, nbs ) =
            List.map (\w -> ( v, w )) nbs
    in
        Array.toIndexedList adj
            |> List.concatMap incident
            |> List.filter (\( v, w ) -> v < w)


verticesByDistance : Int -> Graph -> List (List Int)
verticesByDistance start adj =
    let
        newNeighbors v seen =
            ListHelpers.filterCyclicFromSplit
                (\u -> not (Set.member u seen))
                (neighbors v adj)

        nextLayer seen layers =
            Maybe.withDefault [] (List.head layers)
                |> List.concatMap (\v -> newNeighbors v seen)
                |> ListHelpers.unique

        step seen layers =
            case nextLayer seen layers of
                [] ->
                    layers

                next ->
                    step
                        (List.foldl Set.insert seen next)
                        (next :: layers)
    in
        List.reverse <| step (Set.fromList [ start ]) [ [ start ] ]
