module SurfaceGraph
    exposing
        ( Graph
        , graph
        , neighbors
        , nrVertices
        , edges
        , verticesByDistance
        , addVertex
        , removeEdge
        )

import Array exposing (Array)
import Set exposing (Set)
import ListHelpers


type Graph
    = Graph (Array (List Int))


makeGraph : Array (List Int) -> Graph
makeGraph =
    Array.map ListHelpers.sortCyclic >> Graph


graph : List (List Int) -> Graph
graph =
    Array.fromList >> makeGraph


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


insertBefore : a -> a -> List a -> List a
insertBefore a b aList =
    ListHelpers.indexWhen ((==) a) aList
        |> Maybe.withDefault 0
        |> (\n -> ListHelpers.insertAt n b aList)


addVertex : List Int -> Graph -> Graph
addVertex nbs ((Graph adj) as gr) =
    let
        insert u v =
            insertBefore u (nrVertices gr) (neighbors v gr)
    in
        List.map2 (,) nbs (ListHelpers.cycle 1 nbs)
            |> List.foldl (\( u, v ) -> Array.set v (insert u v)) adj
            |> Array.push nbs
            |> makeGraph


removeEdge : Int -> Int -> Graph -> Graph
removeEdge u v ((Graph adj) as gr) =
    adj
        |> Array.set u (List.filter ((/=) v) (neighbors u gr))
        |> Array.set v (List.filter ((/=) u) (neighbors v gr))
        |> makeGraph
