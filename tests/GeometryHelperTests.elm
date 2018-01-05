module GeometryHelperTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, float, list)
import Test exposing (Test, describe, fuzz, fuzz2)
import Math.Vector3 as Vec3 exposing (Vec3)
import GeometryHelpers


-- custom fuzzers


vec3 : Fuzzer Vec3
vec3 =
    Fuzz.map3 Vec3.vec3 float float float



-- custom expectations


expectWithin :
    Expect.FloatingPointTolerance
    -> Float
    -> Float
    -> Expectation
expectWithin tolerance x y =
    -- compensates for bug when comparing negative numbers
    if x < 0 then
        Expect.within tolerance -x -y
    else
        Expect.within tolerance x y


expectAlmostEqual : Float -> Float -> Expectation
expectAlmostEqual =
    expectWithin (Expect.Relative 1.0e-14)


expectNaN : Float -> Expectation
expectNaN x =
    Expect.true "expected not-a-number" (isNaN x)


expectInf : Float -> Expectation
expectInf x =
    Expect.true "expected infinity" (isInfinite x)


expectAllVec : (Float -> Expectation) -> Vec3 -> Expectation
expectAllVec test v =
    Expect.all
        [ test << Vec3.getX
        , test << Vec3.getY
        , test << Vec3.getZ
        ]
        v


expectAllVec2 :
    (Float -> Float -> Expectation)
    -> Vec3
    -> Vec3
    -> Expectation
expectAllVec2 test v w =
    Expect.all
        [ \( v, w ) -> test (Vec3.getX v) (Vec3.getX w)
        , \( v, w ) -> test (Vec3.getY v) (Vec3.getY w)
        , \( v, w ) -> test (Vec3.getZ v) (Vec3.getZ w)
        ]
        ( v, w )



-- per-function test suites


testsForDiv : List Test
testsForDiv =
    [ fuzz2 int int "computes the correct quotient" <|
        \a b ->
            let
                q =
                    GeometryHelpers.div a b
            in
                if b == 0 then
                    if a == 0 then
                        expectNaN q
                    else
                        expectInf q
                else
                    expectAlmostEqual (toFloat a) (q * (toFloat b))
    ]


testsForAverage : List Test
testsForAverage =
    [ fuzz (list float) "computes the correct average" <|
        \numbers ->
            let
                avg =
                    GeometryHelpers.average numbers
            in
                if List.isEmpty numbers then
                    expectNaN avg
                else
                    expectAlmostEqual
                        (List.sum numbers)
                        (avg * (toFloat (List.length numbers)))
    ]


testsForCenter : List Test
testsForCenter =
    [ fuzz (list vec3) "computes the correct center" <|
        \points ->
            let
                n =
                    toFloat <| List.length points

                c =
                    GeometryHelpers.center points

                s =
                    List.foldl Vec3.add (Vec3.vec3 0 0 0) points
            in
                if List.isEmpty points then
                    expectAllVec expectNaN c
                else
                    expectAllVec2 expectAlmostEqual s (Vec3.scale n c)
    ]



-- the main test suite


suite : Test
suite =
    describe "The GeometryHelpers module"
        [ describe "GeometryHelpers.div" testsForDiv
        , describe "GeometryHelpers.average" testsForAverage
        , describe "GeometryHelpers.center" testsForCenter
        ]
