module Embed exposing (Adjacencies, Embedding, Embedder, tutte)

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)


type alias Adjacencies =
    Array (List Int)


type alias Embedding =
    Array Vec3


type alias Embedder =
    Adjacencies -> Embedding


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


nGon : Int -> List Vec3
nGon n =
    let
        angle i =
            2 * pi * (toFloat i) / (toFloat n)

        corner i =
            vec3 (cos (angle i)) (sin (angle i)) 0.0
    in
        List.map corner (List.range 0 (n - 1))


tutteInitial : List Int -> Adjacencies -> Embedding
tutteInitial outer adj =
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


center : List Vec3 -> Vec3
center points =
    Vec3.normalize (List.foldl Vec3.add (vec3 0 0 0) points)


amap2 : (a -> b -> c) -> Array a -> Array b -> Array c
amap2 fn xs ys =
    Array.fromList (List.map2 fn (Array.toList xs) (Array.toList ys))


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
                    (tutteInitial f adj)
                    (List.range 1 20)

            Nothing ->
                Array.empty
