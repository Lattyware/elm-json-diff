module CheapDiff exposing (suite)

import Cases exposing (..)
import Expect exposing (Expectation)
import Json.Diff as Diff
import Json.Encode as JsonE
import Json.Patch as Json
import Json.Patch.Invertible as Invertible
import Test exposing (..)


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
            in
            case Diff.cheapDiff a b |> Invertible.toMinimalPatch |> applyPatchToA of
                Ok hopefullyB ->
                    expectJsonEqual b hopefullyB

                Err err ->
                    Expect.fail ("Couldn't apply patch: " ++ err)
    in
    test (description ++ " | a: " ++ (a |> JsonE.encode 0) ++ " | b: " ++ (b |> JsonE.encode 0))
        doTest


expectJsonEqual : JsonE.Value -> JsonE.Value -> Expectation
expectJsonEqual a =
    Expect.all
        [ Expect.equal a
        , \b -> Expect.equal b a
        ]
