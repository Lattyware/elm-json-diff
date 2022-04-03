module Invertible exposing (invertSuite, mergeSuite)

import Cases exposing (TestCase, suiteUsingTestCases)
import Expect exposing (Expectation)
import Json.Diff as Diff
import Json.Encode as JsonE
import Json.Patch.Invertible as Invertable
import Test exposing (..)
import Util.Expect as Expect


invertSuite : Test
invertSuite =
    suiteUsingTestCases "Patch, invert, patch has the same result for: " invertTest


invertTest : TestCase -> Test
invertTest { a, b } =
    test ("a: " ++ (a |> JsonE.encode 0) ++ " | b: " ++ (b |> JsonE.encode 0))
        (\_ ->
            let
                diff =
                    Diff.invertibleDiff a b

                inverted =
                    diff |> Invertable.invert
            in
            case a |> Invertable.apply diff |> Result.andThen (Invertable.apply inverted) of
                Ok aAgain ->
                    Expect.jsonEqual a aAgain

                Err err ->
                    Expect.fail err
        )


mergeSuite : Test
mergeSuite =
    suiteUsingTestCases "Patch and Merge Patch have the same result for:" mergeTest


mergeTest : TestCase -> Test
mergeTest { a, b } =
    test ("a: " ++ (a |> JsonE.encode 0) ++ " | b: " ++ (b |> JsonE.encode 0))
        (\_ ->
            let
                diff =
                    Diff.invertibleDiff a b

                mergedDiff =
                    diff |> Invertable.merge
            in
            case a |> Invertable.apply diff of
                Ok patched ->
                    case a |> Invertable.apply mergedDiff of
                        Ok mergePatched ->
                            Expect.jsonEqual patched mergePatched

                        Err err ->
                            Expect.fail err

                Err err ->
                    Expect.fail err
        )
