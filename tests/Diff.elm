module Diff exposing (suite)

import Cases exposing (..)
import Expect exposing (Expectation)
import Json.Diff as Diff
import Json.Encode as JsonE
import Json.Patch as Json
import Test exposing (..)
import Util.Expect as Expect


suite : Test
suite =
    suiteUsingTestCases "Diff.diff correctly handles" toTest


toTest : TestCase -> Test
toTest { description, a, b, patch } =
    test (description ++ " | a: " ++ (a |> JsonE.encode 0) ++ " | b: " ++ (b |> JsonE.encode 0) ++ " | patch: " ++ (patch |> Json.encoder |> JsonE.encode 0))
        (\_ -> Diff.diff a b |> expectPatchEqual patch)


expectPatchEqual : Json.Patch -> Json.Patch -> Expectation
expectPatchEqual a b =
    Expect.jsonEqual (Json.encoder a) (Json.encoder b)
