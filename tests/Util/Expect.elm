module Util.Expect exposing (jsonEqual)

import Expect exposing (Expectation)
import Json.Encode as JsonE


jsonEqual : JsonE.Value -> JsonE.Value -> Expectation
jsonEqual a b =
    Expect.all
        [ Expect.equal a
        , \c -> Expect.equal c a
        ]
        b
        |> Expect.onFail ("Expect.jsonEqual\nExpected: " ++ JsonE.encode 0 a ++ "\nActual: " ++ JsonE.encode 0 b)
