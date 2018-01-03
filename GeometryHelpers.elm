module GeometryHelpers
    exposing
        ( div
        , average
        , center
        , ngonAngles
        , pointOnSphere
        , limitDisplacement
        )

import Math.Vector3 as Vec3 exposing (vec3, Vec3)


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
