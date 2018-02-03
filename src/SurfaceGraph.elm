module SurfaceGraph
    exposing
        ( Graph
        , tetrahedron
        , graph
        , neighbors
        , degree
        , nrVertices
        , directedEdges
        , edges
        , verticesByDistance
        , face
        , faces
        , addVertex
        , triangulateFaceFromCenter
        , removableEdge
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


tetrahedron : Graph
tetrahedron =
    [ [ 1, 2, 3 ], [ 0, 3, 2 ], [ 0, 1, 3 ], [ 0, 2, 1 ] ]
        |> Array.fromList
        |> makeGraph


graph : List (List Int) -> Maybe Graph
graph =
    Array.fromList >> makeGraph >> Just


neighbors : Int -> Graph -> List Int
neighbors v (Graph adj) =
    Maybe.withDefault [] (Array.get v adj)


degree : Int -> Graph -> Int
degree v =
    neighbors v >> List.length


nrVertices : Graph -> Int
nrVertices (Graph adj) =
    Array.length adj


directedEdges : Graph -> List ( Int, Int )
directedEdges (Graph adj) =
    let
        incident ( v, nbs ) =
            List.map (\w -> ( v, w )) nbs
    in
        Array.toIndexedList adj |> List.concatMap incident


edges : Graph -> List ( Int, Int )
edges =
    directedEdges >> List.filter (\( v, w ) -> v < w)


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


cyclicSuccessor : a -> List a -> Maybe a
cyclicSuccessor a aList =
    ListHelpers.indexWhen ((==) a) aList
        |> Maybe.andThen (\i -> ListHelpers.pickCyclic (i + 1) aList)


face : Int -> Int -> Graph -> List Int
face v0 w0 gr =
    let
        step v w vs =
            case cyclicSuccessor w (neighbors v gr) of
                Nothing ->
                    []

                Just u ->
                    if u == v0 then
                        u :: vs
                    else
                        step u v (u :: vs)
    in
        step v0 w0 []


faces : Graph -> List (List Int)
faces gr =
    let
        step edgesLeft facesSoFar =
            case List.head edgesLeft of
                Nothing ->
                    List.reverse facesSoFar

                Just ( v, w ) ->
                    let
                        f =
                            face v w gr

                        edgesInF =
                            ListHelpers.cyclicPairs f

                        es =
                            ListHelpers.diffLists edgesLeft edgesInF
                    in
                        step es (f :: facesSoFar)
    in
        step (directedEdges gr) []


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
        ListHelpers.cyclicPairs nbs
            |> List.foldl (\( u, v ) -> Array.set v (insert u v)) adj
            |> Array.push nbs
            |> makeGraph


triangulateFaceFromCenter : Int -> Int -> Graph -> Graph
triangulateFaceFromCenter v w gr =
    addVertex (face v w gr) gr


removableEdge : Int -> Int -> Graph -> Bool
removableEdge v w gr =
    (degree v gr > 3)
        && (degree w gr > 3)
        && (ListHelpers.intersectLists (face v w gr) (face w v gr)
                |> List.length
                |> (==) 2
           )


removeEdge : Int -> Int -> Graph -> Graph
removeEdge u v ((Graph adj) as gr) =
    adj
        |> Array.set u (List.filter ((/=) v) (neighbors u gr))
        |> Array.set v (List.filter ((/=) u) (neighbors v gr))
        |> makeGraph
