module Invertible exposing (suite)

import Cases exposing (TestCase, suiteUsingTestCases)
import Expect exposing (Expectation)
import Json.Decode as Json
import Json.Diff as Diff
import Json.Encode as JsonE
import Json.Patch as Json
import Json.Patch.Invertible as Invertable
import Test exposing (..)


suite : Test
suite =
    suiteUsingTestCases "Invertible correctly handles " toTest


toTest : TestCase -> Test
toTest { a, b } =
    test ("Patch and back works | a: " ++ (a |> JsonE.encode 0) ++ " | b: " ++ (b |> JsonE.encode 0))
        (\_ ->
            let
                diff =
                    Diff.invertibleDiff a b

                inverted =
                    diff |> Invertable.invert
            in
            case a |> Invertable.apply diff |> Result.andThen (Invertable.apply inverted) of
                Ok aAgain ->
                    expectJsonEqual a aAgain

                Err err ->
                    Expect.fail err
        )


expectJsonEqual : Json.Value -> Json.Value -> Expectation
expectJsonEqual a =
    Expect.all
        [ Expect.equal a
        , \b -> Expect.equal b a
        ]
