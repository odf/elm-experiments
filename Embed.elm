module Embed exposing (Adjacencies, Embedding, Embedder, default)

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)


-- Data types


type alias Adjacencies =
    Array (List Int)


type alias Embedding =
    Array Vec3


type alias Embedder =
    Adjacencies -> Embedding


type alias Placer =
    Embedding -> Adjacencies -> Int -> Vec3


type alias Cooler =
    Int -> Int -> Float



-- List and array manipulation helpers


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



-- Graph manipulation helpers


face : Int -> Int -> Adjacencies -> Maybe (List Int)
face v0 w0 adj =
    let
        step v w rest =
            case Maybe.andThen (nextCyclic w) (Array.get v adj) of
                Nothing ->
                    Nothing

                Just u ->
                    if u == v0 || List.length rest > Array.length adj then
                        Just (w :: rest)
                    else
                        step u v (w :: rest)
    in
        step v0 w0 []


firstFace : Adjacencies -> Maybe (List Int)
firstFace adj =
    Array.get 0 adj
        |> Maybe.andThen (\vs -> List.head vs)
        |> Maybe.andThen (\w -> face 0 w adj)


getNeighbors : Int -> Adjacencies -> List Int
getNeighbors v adj =
    Maybe.withDefault [] (Array.get v adj)


-- Geometry helpers


nGon : Int -> List Vec3
nGon n =
    let
        angle i =
            2 * pi * (toFloat i) / (toFloat n)

        corner i =
            vec3 (cos (angle i)) (sin (angle i)) 0.0
    in
        List.map corner (List.range 0 (n - 1))


limitDistance : Float -> Vec3 -> Vec3 -> Vec3
limitDistance t vNew vOld =
    let
        d =
            Vec3.distanceSquared vNew vOld
    in
        if d > t then
            Vec3.add vOld (Vec3.scale (t / d) (Vec3.sub vNew vOld))
        else
            vNew


-- Generic embedding code


getPos : Int -> Embedding -> Vec3
getPos v pos =
    Maybe.withDefault (vec3 0 0 0) (Array.get v pos)


iterate :
    Placer
    -> Int
    -> Float
    -> Cooler
    -> Embedding
    -> Adjacencies
    -> Embedding
iterate placer nrSteps limit cooler positions adj =
    let
        n =
            Array.length adj

        verts =
            Array.fromList <| List.range 0 (n - 1)

        step i pos =
            let
                temperature =
                    cooler i nrSteps

                update v =
                    limitDistance temperature (placer pos adj v) (getPos v pos)

                newPos =
                    Array.map update verts
            in
                newPos
    in
        List.foldl step positions (List.range 1 nrSteps)


fastCooler : Cooler
fastCooler step maxStep =
    0.1 * (1.0 - (toFloat step) / (toFloat maxStep))


slowCooler : Cooler
slowCooler step maxStep =
    0.04 * (1.0 - (toFloat step) / (toFloat maxStep)) ^ 3


simpleEmbedding : List Int -> Adjacencies -> Embedding
simpleEmbedding outer adj =
    let
        setValue ( idx, val ) a =
            Array.set idx val a

        init =
            (Array.initialize (Array.length adj) (\_ -> vec3 0 0 1))

        alpha =
            pi / 16

        shifted v =
            Vec3.sub (Vec3.scale (sin alpha) v) (vec3 0 0 (cos alpha))

        specs =
            List.map2 (,) outer (List.map shifted (nGon (List.length outer)))
    in
        List.foldl setValue init specs


init : Adjacencies -> Embedding
init adj =
    case firstFace adj of
        Nothing ->
            Array.map (\_ -> (vec3 0 0 0)) adj

        Just f ->
            simpleEmbedding f adj


embed :
    Embedder
    -> Placer
    -> Int
    -> Float
    -> Cooler
    -> Adjacencies
    -> Embedding
embed init placer nrSteps limit cooler adj =
    iterate placer nrSteps limit cooler (init adj) adj


-- Specific embedding code


sphericalPlacer : Placer
sphericalPlacer pos adj v =
    let
        p =
            getPos v pos

        weightedPos w =
            let
                q = getPos w pos

                d = Vec3.distanceSquared p q
            in
                Vec3.scale d q

        normalizedSum points =
            let
                s = List.foldl Vec3.add (vec3 0 0 0) points
            in
                if (Vec3.length s) < 1e-8 then
                    s
                else
                    Vec3.normalize s

    in
        normalizedSum (List.map weightedPos (getNeighbors v adj))


spherical : Adjacencies -> Embedding
spherical adj =
    embed init sphericalPlacer 500 1e-4 fastCooler adj


default : Embedder
default = spherical
