module GeometryHelperTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (int, float, list)
import Test exposing (Test, describe, fuzz, fuzz2)
import GeometryHelpers


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
        \list ->
            let
                avg =
                    GeometryHelpers.average list
            in
                if List.isEmpty list then
                    expectNaN avg
                else
                    expectAlmostEqual
                        (List.sum list)
                        (avg * (toFloat (List.length list)))
    ]


suite : Test
suite =
    describe "The GeometryHelpers module"
        [ describe "GeometryHelpers.div" testsForDiv
        , describe "GeometryHelpers.average" testsForAverage
        ]
