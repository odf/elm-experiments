module Embed exposing (Adjacencies, Embedding, Embedder, tutte)

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


amap2 : (a -> b -> c) -> Array a -> Array b -> Array c
amap2 fn xs ys =
    Array.fromList (List.map2 fn (Array.toList xs) (Array.toList ys))



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



-- Geometry helpers


center : List Vec3 -> Vec3
center points =
    Vec3.normalize (List.foldl Vec3.add (vec3 0 0 0) points)


nGon : Int -> List Vec3
nGon n =
    let
        angle i =
            2 * pi * (toFloat i) / (toFloat n)

        corner i =
            vec3 (cos (angle i)) (sin (angle i)) 0.0
    in
        List.map corner (List.range 0 (n - 1))



-- Generic embedding code


initialEmbedding : List Int -> Adjacencies -> Embedding
initialEmbedding outer adj =
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

                newPos =
                    Array.map (placer pos adj) verts
            in
                newPos
    in
        List.foldl step positions (List.range 1 nrSteps)


getPos : Embedding -> Int -> Vec3
getPos pos v =
    Maybe.withDefault (vec3 0 0 0) (Array.get v pos)


fastCooler : Cooler
fastCooler step maxStep =
    0.1 * (1.0 - (toFloat step) / (toFloat maxStep))


slowCooler : Cooler
slowCooler step maxStep =
    0.04 * (1.0 - (toFloat step) / (toFloat maxStep)) ^ 3



-- Specific embedding code


tuttePlacer : Placer
tuttePlacer pos adj v =
    let
        p =
            getPos pos v

        vs =
            Maybe.withDefault [] (Array.get v adj)

        moved p q =
            center [ p, center [ p, q ] ]
    in
        center (List.map (\w -> moved p (getPos pos w)) vs)


tutteStep : Embedding -> Adjacencies -> Embedding
tutteStep pos adj =
    let
        getPos v =
            Maybe.withDefault (vec3 0 0 0) (Array.get v pos)

        moved p q =
            center [ p, center [ p, q ] ]

        newPos ( p, vs ) =
            center (List.map (\v -> moved p (getPos v)) vs)
    in
        Array.map newPos (amap2 (,) pos adj)


tutte : Adjacencies -> Embedding
tutte adj =
    let
        next _ positions =
            tutteStep positions adj
    in
        case firstFace adj of
            Just f ->
                List.foldl
                    next
                    (initialEmbedding f adj)
                    (List.range 1 20)

            Nothing ->
                Array.empty
