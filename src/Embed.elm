module Embed
    exposing
        ( Embedding
        , Embedder
        , embedding
        , getPos
        , init
        , spherical
        , molecular
        )

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import ListHelpers
import GeometryHelpers
import SurfaceGraph exposing (Graph)


-- Data types


type Embedding
    = Embedding (Array Vec3)


type alias Embedder =
    Graph -> Embedding


type alias Placer =
    Embedding -> Graph -> Int -> Vec3


type alias Normalizer =
    Embedding -> Embedding


type alias Cooler =
    Int -> Int -> Float



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


averageEdgeLength : Graph -> Embedding -> Float
averageEdgeLength adj pos =
    SurfaceGraph.edges adj
        |> List.map
            (\( v, w ) -> Vec3.distance (getPos v pos) (getPos w pos))
        |> GeometryHelpers.average


scaleBy : Float -> Embedding -> Embedding
scaleBy factor =
    map (Vec3.scale factor)


normalizeTo : Float -> Graph -> Embedding -> Embedding
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
    -> Graph
    -> Embedding
    -> Embedding
iterate place nrSteps limit temperature adj positions =
    let
        verts =
            List.range 0 (SurfaceGraph.nrVertices adj - 1)

        update pos i v =
            GeometryHelpers.limitDisplacement
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
    factor * (1 - (GeometryHelpers.div step maxStep)) ^ exponent


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
        List.range 0 (n - 1)
            |> List.map (\i -> pi * (GeometryHelpers.div i m))


init : Embedder
init adj =
    let
        layers =
            SurfaceGraph.verticesByDistance 0 adj

        rings =
            List.map
                (\vs -> GeometryHelpers.ngonAngles (List.length vs))
                layers

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
            |> List.map
                (\( v, phi, theta ) ->
                    GeometryHelpers.pointOnSphere phi theta
                )
            |> embedding



-- Specific vertex placers


sphericalPlacer : Placer
sphericalPlacer pos adj v =
    let
        p =
            getPos v pos

        s =
            SurfaceGraph.neighbors v adj
                |> List.map (\w -> getPos w pos)
                |> List.map
                    (\q -> Vec3.scale (Vec3.distanceSquared p q) q)
                |> GeometryHelpers.center
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
            SurfaceGraph.neighbors v adj
                |> List.map (\w -> getPos w pos)

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
    -> Graph
    -> Int
    -> Int
    -> List ( Vec3, Float )
pointsAndWeights pos adj v w =
    let
        rest =
            ListHelpers.filterCyclicFromSplit
                (\u -> u /= v)
                (SurfaceGraph.neighbors w adj)

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
            SurfaceGraph.neighbors v adj
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
