module CheapDiff exposing (suite)

import Cases exposing (..)
import Expect exposing (Expectation)
import Json.Diff as Diff
import Json.Encode as JsonE
import Json.Patch as Json
import Json.Patch.Invertible as Invertible
import Test exposing (..)
import Util.Expect as Expect


suite : Test
suite =
    suiteUsingTestCases "Diff.cheapDiff correctly handles" toTest


toTest : TestCase -> Test
toTest { description, a, b, patch } =
    let
        doTest _ =
            let
                applyPatchToA p =
                    Json.apply p a

                diffedPatch =
                    Diff.cheapDiff a b |> Invertible.toMinimalPatch
            in
            case diffedPatch |> applyPatchToA of
                Ok hopefullyB ->
                    Expect.jsonEqual b hopefullyB

                Err err ->
                    Expect.fail ("Couldn't apply patch:\n\tPatch: " ++ (diffedPatch |> Json.encoder |> JsonE.encode 0) ++ "\n\tError: " ++ err)
    in
    test (description ++ " | a: " ++ (a |> JsonE.encode 0) ++ " | b: " ++ (b |> JsonE.encode 0))
        doTest
