module Embed
    exposing
        ( Adjacencies
        , Embedding
        , Embedder
        , adjacencies
        , getNeighbors
        , getPos
        , edges
        , default
        )

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


type alias Normalizer =
    Embedding -> Embedding


type alias Cooler =
    Int -> Int -> Float



-- List and array helpers


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



-- Graph helpers


adjacencies : List (List Int) -> Adjacencies
adjacencies =
    Array.fromList


getNeighbors : Int -> Adjacencies -> List Int
getNeighbors v adj =
    Maybe.withDefault [] (Array.get v adj)


face : Int -> Int -> Adjacencies -> Maybe (List Int)
face v0 w0 adj =
    let
        step v w rest =
            case nextCyclic w <| getNeighbors v adj of
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
    List.head (getNeighbors 0 adj)
        |> Maybe.andThen (\w -> face 0 w adj)


edges : Adjacencies -> List ( Int, Int )
edges adj =
    let
        incident ( v, nbs ) =
            List.map (\w -> ( v, w )) nbs
    in
        List.concat <| List.map incident <| Array.toIndexedList adj



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
    -> Normalizer
    -> Int
    -> Float
    -> Cooler
    -> Embedding
    -> Adjacencies
    -> Embedding
iterate place normalize nrSteps limit cool positions adj =
    let
        n =
            Array.length adj

        verts =
            Array.fromList <| List.range 0 (n - 1)

        step i pos =
            let
                temperature =
                    cool i nrSteps

                update v =
                    limitDistance temperature (place pos adj v) (getPos v pos)
            in
                normalize <| Array.map update verts
    in
        List.foldl step positions (List.range 1 nrSteps)


embed :
    Embedder
    -> Placer
    -> Normalizer
    -> Int
    -> Float
    -> Cooler
    -> Adjacencies
    -> Embedding
embed init placer normalizer nrSteps limit cooler adj =
    iterate placer normalizer nrSteps limit cooler (init adj) adj


sphericalNormalizer : Embedding -> Embedding
sphericalNormalizer pos =
    let
        center =
            Vec3.scale (1 / (toFloat <| Array.length pos)) <|
                List.foldl Vec3.add (vec3 0 0 0) (Array.toList pos)
    in
        Array.map (\p -> Vec3.normalize (Vec3.sub p center)) pos


genericCooler : Float -> Float -> Cooler
genericCooler factor exponent step maxStep =
    factor * (1 - (toFloat step) / (toFloat maxStep)) ^ exponent


simpleEmbedding : List Int -> Adjacencies -> Embedding
simpleEmbedding outer adj =
    let
        setValue ( idx, val ) a =
            Array.set idx val a

        init =
            (Array.initialize (Array.length adj) (\_ -> vec3 0 0 1))

        alpha =
            pi / 3

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



-- Specific embedding code


sphericalPlacer : Placer
sphericalPlacer pos adj v =
    let
        p =
            getPos v pos

        weightedPos w =
            let
                q =
                    getPos w pos

                d =
                    Vec3.distanceSquared p q
            in
                Vec3.scale d q

        normalizedSum points =
            let
                s =
                    List.foldl Vec3.add (vec3 0 0 0) points
            in
                if (Vec3.length s) < 1.0e-8 then
                    Vec3.normalize p
                else
                    Vec3.normalize s
    in
        normalizedSum (List.map weightedPos (getNeighbors v adj))


spherical : Adjacencies -> Embedding
spherical adj =
    let
        cooler =
            genericCooler 0.1 3
    in
        embed init sphericalPlacer sphericalNormalizer 50 1.0e-4 cooler adj


default : Embedder
default =
    spherical
