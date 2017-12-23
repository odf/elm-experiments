module Embed
    exposing
        ( Adjacencies
        , Embedding
        , Embedder
        , adjacencies
        , getPos
        , edges
        , default
        )

import Array exposing (Array)
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
        List.filter (\( v, w ) -> v < w) <|
            List.concat <|
                List.map incident <|
                    Array.toIndexedList adj


face : Int -> Int -> Adjacencies -> Maybe (List Int)
face v0 w0 adj =
    let
        step v w m rest =
            if m >= nrVertices adj then
                Nothing
            else
                case nextCyclic w (neighbors v adj) of
                    Nothing ->
                        Nothing

                    Just u ->
                        if u == v0 then
                            Just (w :: rest)
                        else
                            step u v (m + 1) (w :: rest)
    in
        step v0 w0 0 []


firstFace : Adjacencies -> Maybe (List Int)
firstFace adj =
    List.head (neighbors 0 adj)
        |> Maybe.andThen (\w -> face 0 w adj)



-- Geometry helpers


center : List Vec3 -> Vec3
center points =
    let
        n =
            List.length points

        sum =
            List.foldl Vec3.add (vec3 0 0 0) points
    in
        Vec3.scale (1 / (toFloat n)) sum


nGon : Int -> List Vec3
nGon n =
    let
        corner i =
            let
                alpha =
                    2 * pi * (toFloat i) / (toFloat n)
            in
                vec3 (cos alpha) (sin alpha) 0.0
    in
        List.map corner (List.range 0 (n - 1))


limitDisplacement : Float -> Vec3 -> Vec3 -> Vec3
limitDisplacement limit vNew vOld =
    let
        dist =
            Vec3.distanceSquared vNew vOld
    in
        if dist > limit then
            Vec3.add vOld (Vec3.scale (limit / dist) (Vec3.sub vNew vOld))
        else
            vNew



-- Generic embedding code


getPos : Int -> Embedding -> Vec3
getPos v (Embedding pos) =
    Maybe.withDefault (vec3 0 0 0) (Array.get v pos)


map : (Vec3 -> Vec3) -> Embedding -> Embedding
map fn (Embedding pos) =
    Embedding (Array.map fn pos)


iterate :
    Placer
    -> Normalizer
    -> Int
    -> Float
    -> Cooler
    -> Embedding
    -> Adjacencies
    -> Embedding
iterate place normalize nrSteps limit temperature positions adj =
    let
        n =
            nrVertices adj

        verts =
            Array.fromList <| List.range 0 (n - 1)

        step i pos =
            let
                update v =
                    limitDisplacement
                        (temperature i nrSteps)
                        (place pos adj v)
                        (getPos v pos)
            in
                normalize <| Embedding <| Array.map update verts
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
sphericalNormalizer (Embedding pos) =
    let
        c =
            center (Array.toList pos)
    in
        Embedding (Array.map (\p -> Vec3.normalize (Vec3.sub p c)) pos)


genericCooler : Float -> Float -> Cooler
genericCooler factor exponent step maxStep =
    factor * (1 - (toFloat step) / (toFloat maxStep)) ^ exponent


init : Adjacencies -> Embedding
init adj =
    (case firstFace adj of
        Nothing ->
            Array.repeat (nrVertices adj) (vec3 0 0 0)

        Just outer ->
            List.foldl
                (\( idx, val ) -> Array.set idx val)
                (Array.repeat (nrVertices adj) (vec3 0 0 1))
                (List.map2 (,) outer (nGon (List.length outer)))
    )
        |> Embedding



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
            in
                Vec3.scale (Vec3.distanceSquared p q) q

        s =
            center <| List.map weightedPos <| neighbors v adj
    in
        if (Vec3.length s) < 1.0e-8 then
            Vec3.normalize p
        else
            Vec3.normalize s


spherical : Adjacencies -> Embedding
spherical =
    embed init sphericalPlacer sphericalNormalizer 50 1.0e-4 <|
        genericCooler 0.1 3


default : Embedder
default =
    spherical
