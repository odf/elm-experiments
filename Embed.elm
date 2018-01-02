module Embed
    exposing
        ( Adjacencies
        , Embedding
        , Embedder
        , adjacencies
        , embedding
        , getPos
        , edges
        , init
        , spherical
        , molecular
        )

import Array exposing (Array)
import Set exposing (Set)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)


-- Data types


type Adjacencies
    = Adjacencies (Array (List Int))


type Embedding
    = Embedding (Array Vec3)


type alias Embedder =
    Adjacencies -> Embedding


type alias Placer =
    Embedding -> Adjacencies -> Int -> Vec3


type alias Normalizer =
    Embedding -> Embedding


type alias Cooler =
    Int -> Int -> Float



-- List helpers


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


filterAndCyclicReorder : (a -> Bool) -> List a -> List a
filterAndCyclicReorder pred aList =
    let
        ( leading, trailing ) =
            splitWhen (\a -> not (pred a)) aList
    in
        List.filter pred (trailing ++ leading)



-- Graph helpers


adjacencies : List (List Int) -> Adjacencies
adjacencies =
    Adjacencies << Array.fromList


neighbors : Int -> Adjacencies -> List Int
neighbors v (Adjacencies adj) =
    Maybe.withDefault [] (Array.get v adj)


nrVertices : Adjacencies -> Int
nrVertices (Adjacencies adj) =
    Array.length adj


edges : Adjacencies -> List ( Int, Int )
edges (Adjacencies adj) =
    let
        incident ( v, nbs ) =
            List.map (\w -> ( v, w )) nbs
    in
        Array.toIndexedList adj
            |> List.concatMap incident
            |> List.filter (\( v, w ) -> v < w)


newNeighbors : Int -> Set Int -> Adjacencies -> List Int
newNeighbors v seen adj =
    filterAndCyclicReorder
        (\u -> not (Set.member u seen))
        (neighbors v adj)


verticesByDistance : Int -> Adjacencies -> List (List Int)
verticesByDistance start adj =
    let
        nextLayer seen layers =
            Maybe.withDefault [] (List.head layers)
                |> List.concatMap (\v -> newNeighbors v seen adj)
                |> unique

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



-- Geometry and arithmetic helpers


div : Int -> Int -> Float
div a b =
    (toFloat a) / (toFloat b)


average : List Float -> Float
average fList =
    List.sum fList * (div 1 (List.length fList))


center : List Vec3 -> Vec3
center points =
    List.foldl Vec3.add (vec3 0 0 0) points
        |> Vec3.scale (div 1 (List.length points))


ngonAngles : Int -> List Float
ngonAngles m =
    List.range 1 m |> List.map (\i -> 2 * pi * (div i m))


pointOnSphere : Float -> Float -> Vec3
pointOnSphere phi theta =
    vec3
        ((sin theta) * (cos phi))
        ((sin theta) * (sin phi))
        (cos theta)


limitDisplacement : Float -> Vec3 -> Vec3 -> Vec3
limitDisplacement limit vNew vOld =
    let
        dist =
            Vec3.distanceSquared vNew vOld
    in
        if dist > limit then
            Vec3.add
                vOld
                (Vec3.scale (limit / dist) (Vec3.sub vNew vOld))
        else
            vNew



-- Generic embedding code


embedding : List Vec3 -> Embedding
embedding =
    Embedding << Array.fromList


getPos : Int -> Embedding -> Vec3
getPos v (Embedding pos) =
    Maybe.withDefault (vec3 0 0 0) (Array.get v pos)


map : (Vec3 -> Vec3) -> Embedding -> Embedding
map fn (Embedding pos) =
    Embedding (Array.map fn pos)


pointList : Embedding -> List Vec3
pointList (Embedding p) =
    Array.toList p


averageEdgeLength : Adjacencies -> Embedding -> Float
averageEdgeLength adj pos =
    edges adj
        |> List.map
            (\( v, w ) -> Vec3.distance (getPos v pos) (getPos w pos))
        |> average


scaleBy : Float -> Embedding -> Embedding
scaleBy factor =
    map (Vec3.scale factor)


normalizeTo : Float -> Adjacencies -> Embedding -> Embedding
normalizeTo len adj pos =
    scaleBy (len / averageEdgeLength adj pos) pos


distance : Embedding -> Embedding -> Float
distance pos1 pos2 =
    List.map2 Vec3.distanceSquared (pointList pos1) (pointList pos2)
        |> List.sum
        |> sqrt


iterate :
    Placer
    -> Int
    -> Float
    -> Cooler
    -> Adjacencies
    -> Embedding
    -> Embedding
iterate place nrSteps limit temperature adj positions =
    let
        verts =
            List.range 0 (nrVertices adj - 1)

        update pos i v =
            limitDisplacement
                (temperature i nrSteps)
                (place pos adj v)
                (getPos v pos)

        step i pos =
            let
                next =
                    embedding (List.map (update pos i) verts)
            in
                if i >= nrSteps || distance pos next < limit then
                    next
                else
                    step (i + 1) next
    in
        step 0 positions


genericCooler : Float -> Float -> Cooler
genericCooler factor exponent step maxStep =
    factor * (1 - (div step maxStep)) ^ exponent


ringAngles : List (List Int) -> List Float
ringAngles layers =
    let
        n =
            List.length layers

        last =
            Maybe.withDefault [] (List.head (List.reverse layers))

        m =
            if List.length last > 1 then
                n
            else
                n - 1
    in
        List.range 0 (n - 1) |> List.map (\i -> pi * (div i m))


init : Embedder
init adj =
    let
        layers =
            verticesByDistance 0 adj

        rings =
            List.map (\vs -> ngonAngles (List.length vs)) layers

        ringThetas =
            ringAngles layers

        ringShifts =
            -- TODO implement me
            List.repeat (List.length layers) 0

        makeSpecs vs phis theta shift =
            List.map2 (\v phi -> ( v, phi + shift, theta )) vs phis
    in
        List.map4 makeSpecs layers rings ringThetas ringShifts
            |> List.concat
            |> List.sort
            |> List.map (\( v, phi, theta ) -> pointOnSphere phi theta)
            |> embedding



-- Specific vertex placers


sphericalPlacer : Placer
sphericalPlacer pos adj v =
    let
        p =
            getPos v pos

        s =
            neighbors v adj
                |> List.map (\w -> getPos w pos)
                |> List.map
                    (\q -> Vec3.scale (Vec3.distanceSquared p q) q)
                |> center
    in
        if Vec3.length s < 1.0e-8 then
            Vec3.normalize p
        else
            Vec3.normalize s


centralRepulsionPlacer : Placer
centralRepulsionPlacer pos adj v =
    let
        posV =
            getPos v pos

        wgtV =
            1 / (Vec3.lengthSquared posV)

        posNbs =
            List.map (\w -> getPos w pos) <| neighbors v adj

        wgtNbs =
            List.map (Vec3.distanceSquared posV) posNbs

        sumWgt =
            List.foldl (+) wgtV wgtNbs
    in
        if sumWgt < 1.0e-8 then
            posV
        else
            List.map2 Vec3.scale wgtNbs posNbs
                |> List.foldl Vec3.add (Vec3.scale wgtV posV)
                |> Vec3.scale (1 / sumWgt)


pointsAndWeights :
    Embedding
    -> Adjacencies
    -> Int
    -> Int
    -> List ( Vec3, Float )
pointsAndWeights pos adj v w =
    let
        rest =
            filterAndCyclicReorder (\u -> u /= v) (neighbors w adj)

        pointAndWeight u =
            ( getPos u pos
            , -0.5 / Vec3.distanceSquared (getPos v pos) (getPos u pos)
            )
    in
        [ ( getPos w pos, 1 )
        , pointAndWeight
            (Maybe.withDefault w <| List.head <| List.reverse rest)
        , pointAndWeight
            (Maybe.withDefault w <| List.head rest)
        ]


localRepulsionPlacer : Placer
localRepulsionPlacer pos adj v =
    let
        ( points, weights ) =
            neighbors v adj
                |> List.concatMap (pointsAndWeights pos adj v)
                |> List.unzip
    in
        if List.sum weights < 1.0e-8 then
            getPos v pos
        else
            List.map2 Vec3.scale weights points
                |> List.foldl Vec3.add (vec3 0 0 0)
                |> Vec3.scale (1 / List.sum weights)



-- Complete embedders


spherical : Embedder
spherical adj =
    let
        limit =
            1.0e-2

        cooler =
            genericCooler 0.1 3
    in
        init adj
            |> iterate sphericalPlacer 500 limit cooler adj
            |> normalizeTo 1 adj


molecular : Embedder
molecular adj =
    let
        limit =
            1.0e-2

        cooler =
            genericCooler 0.1 3
    in
        init adj
            |> iterate sphericalPlacer 500 limit cooler adj
            |> normalizeTo 0.1 adj
            |> iterate centralRepulsionPlacer 500 limit cooler adj
            |> normalizeTo 1 adj
            |> iterate localRepulsionPlacer 500 limit cooler adj
