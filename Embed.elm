module Embed
    exposing
        ( Adjacencies
        , Embedding
        , Embedder
        , adjacencies
        , getPos
        , edges
        , initSpherical
        , spherical
        , molecular
        )

import Array exposing (Array)
import Set
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


unique : List comparable -> List comparable
unique aList =
    let
        step seen remaining out =
            case remaining of
                [] ->
                    out

                a :: rest ->
                    if Set.member a seen then
                        step seen rest out
                    else
                        step (Set.insert a seen) rest (a :: out)
    in
        step (Set.empty) (List.reverse aList) []



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


verticesByDistance : Int -> Adjacencies -> List (List Int)
verticesByDistance start adj =
    let
        step seen layers =
            let
                next =
                    Maybe.withDefault [] (List.head layers)
                        |> List.map (\v -> neighbors v adj)
                        |> List.concat
                        |> unique
                        |> List.filter (\v -> not (Set.member v seen))
            in
                if List.isEmpty next then
                    layers
                else
                    step
                        (List.foldl Set.insert seen next)
                        (next :: layers)
    in
        List.reverse <| step (Set.fromList [ start ]) [ [ start ] ]


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


averageEdgeLength : Adjacencies -> Embedding -> Float
averageEdgeLength adj pos =
    let
        es =
            edges adj

        edgeLength ( v, w ) =
            Vec3.distance (getPos v pos) (getPos w pos)
    in
        List.sum (List.map edgeLength es) / (toFloat <| List.length es)


scaleBy : Float -> Embedding -> Embedding
scaleBy factor =
    map (Vec3.scale factor)


normalizeTo : Float -> Adjacencies -> Embedding -> Embedding
normalizeTo len adj pos =
    scaleBy (len / averageEdgeLength adj pos) pos


distance : Embedding -> Embedding -> Float
distance pos1 pos2 =
    let
        n =
            (\(Embedding p) -> Array.length p) pos1

        distSquaredFor v =
            Vec3.distanceSquared (getPos v pos1) (getPos v pos2)
    in
        List.range 0 (n - 1)
            |> List.map distSquaredFor
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
        n =
            nrVertices adj

        verts =
            Array.fromList <| List.range 0 (n - 1)

        update pos t v =
            limitDisplacement t (place pos adj v) (getPos v pos)

        step i pos =
            if i >= nrSteps then
                pos
            else
                let
                    t =
                        temperature i nrSteps

                    next =
                        Embedding <| Array.map (update pos t) verts
                in
                    if distance pos next < limit then
                        next
                    else
                        step (i + 1) next
    in
        step 0 positions


genericCooler : Float -> Float -> Cooler
genericCooler factor exponent step maxStep =
    factor * (1 - (toFloat step) / (toFloat maxStep)) ^ exponent


initSpherical : Embedder
initSpherical adj =
    let
        layers =
            verticesByDistance 0 adj

        n =
            List.length layers

        ring i vs =
            let
                phi =
                    pi * ((toFloat i) / (toFloat (n - 1)) - 1)

                adjust v =
                    Vec3.add
                        (Vec3.scale (sin phi) v)
                        (vec3 0 0 (cos phi))
            in
                List.map2
                    (,)
                    vs
                    (List.map adjust (nGon (List.length vs)))
    in
        List.foldl
            (\( idx, val ) -> Array.set idx val)
            (Array.repeat (nrVertices adj) (vec3 0 0 0))
            (List.concat (List.indexedMap ring layers))
            |> Embedding



-- Specific vertex placers


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

        sumPos =
            List.foldl
                Vec3.add
                (Vec3.scale wgtV posV)
                (List.map2 Vec3.scale wgtNbs posNbs)

        sumWgt =
            List.foldl (+) wgtV wgtNbs
    in
        if sumWgt < 1.0e-8 then
            posV
        else
            Vec3.scale (1 / sumWgt) sumPos


pointsAndWeights :
    Embedding
    -> Adjacencies
    -> Int
    -> Int
    -> List ( Vec3, Float )
pointsAndWeights pos adj v w =
    let
        posV =
            getPos v pos

        nbsW =
            neighbors w adj

        pointAndWeight u =
            case u of
                Nothing ->
                    ( posV, 0 )

                Just u ->
                    let
                        posU =
                            getPos u pos
                    in
                        ( posU, -0.5 / Vec3.distanceSquared posV posU )
    in
        [ ( getPos w pos, 1 )
        , pointAndWeight <| nextCyclic v nbsW
        , pointAndWeight <| nextCyclic v <| List.reverse nbsW
        ]


localRepulsionPlacer : Placer
localRepulsionPlacer pos adj v =
    let
        ( points, weights ) =
            neighbors v adj
                |> List.map (pointsAndWeights pos adj v)
                |> List.concat
                |> List.unzip

        sumPos =
            List.foldl
                Vec3.add
                (vec3 0 0 0)
                (List.map2 Vec3.scale weights points)

        sumWgt =
            List.foldl (+) 0 weights
    in
        if sumWgt < 1.0e-8 then
            getPos v pos
        else
            Vec3.scale (1 / sumWgt) sumPos



-- Complete embedders


spherical : Embedder
spherical adj =
    let
        limit =
            1.0e-2

        cooler =
            genericCooler 0.1 3
    in
        initSpherical adj
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
        initSpherical adj
            |> iterate sphericalPlacer 500 limit cooler adj
            |> normalizeTo 0.1 adj
            |> iterate centralRepulsionPlacer 500 limit cooler adj
            |> normalizeTo 1 adj
            |> iterate localRepulsionPlacer 500 limit cooler adj
