module Cases exposing
    ( TestCase
    , meta
    , suiteUsingTestCases
    )

import Expect
import Json.Decode as Json
import Json.Patch as Json
import Test exposing (..)


type alias TestCase =
    { description : String
    , a : Json.Value
    , b : Json.Value
    , patch : Json.Patch
    , skip : Bool
    }


suiteUsingTestCases : String -> (TestCase -> Test) -> Test
suiteUsingTestCases description toTest =
    case testCases of
        Ok tcs ->
            tcs
                |> List.map (\tc -> skipDisabled tc (toTest tc))
                |> describe description

        Err _ ->
            skip <| test "" (\_ -> Expect.fail "")


meta : Test
meta =
    describe "The test cases"
        [ test "decode correctly from JSON" <|
            \_ ->
                Expect.ok testCases
        ]



{- Private -}


skipDisabled : TestCase -> Test -> Test
skipDisabled { skip } test =
    if skip then
        Test.skip test

    else
        test


cases : String
cases =
    """
        [
            {
                "description": "empty everything",
                "a": {},
                "b": {},
                "patch": []
            },
            {
                "description": "same object",
                "a": {"foo": 1},
                "b": {"foo": 1},
                "patch": []
            },
            {
                "description": "same object, different order",
                "a": {"foo": 1, "bar": 2},
                "b": {"bar":2, "foo": 1},
                "patch": []
            },
            {
                "description": "same object, different order in array",
                "a": [{"foo": 1, "bar": 2}],
                "b": [{"bar":2, "foo": 1}],
                "patch": []
            },
            {
                "description": "same object, different order in object",
                "a": {"foo":{"foo": 1, "bar": 2}},
                "b": {"foo":{"bar":2, "foo": 1}},
                "patch": []
            },
            {
                "description": "add to top level array",
                "a": [],
                "b": ["foo"],
                "patch": [{"op": "add", "path": "/0", "value": "foo"}]
            },
            {
                "description": "same array",
                "a": ["foo"],
                "b": ["foo"],
                "patch": []
            },
            {
                "description": "add string (of a number) to an object",
                "a": {},
                "b": {"foo":"1"},
                "patch": [{"op": "add", "path": "/foo", "value": "1"}]
            },
            {
                "description": "add a number to an object",
                "a": {},
                "b": {"foo":1},
                "patch": [{"op": "add", "path": "/foo", "value": 1}]
            },
            {
                "description": "replace root string",
                "a": "foo",
                "b": "bar",
                "patch": [{"op": "replace", "path": "", "value": "bar"}]
            },
            {
                "description": "replace root object with root array",
                "a": {},
                "b": [],
                "patch": [{"op": "replace", "path": "", "value": []}]
            },
            {
                "description": "replace root array with root object",
                "a": [],
                "b": {},
                "patch": [{"op": "replace", "path": "", "value": {}}]
            },
            {
                "description": "add to root array",
                "a": [],
                "b": ["hi"],
                "patch": [{"op": "add", "path": "/0", "value": "hi"}]
            },
            {
                "description": "add to empty string field",
                "a": {},
                "b": {"":1},
                "patch": [ {"op": "add", "path": "/", "value":1 } ]
            },
            {
                "description": "add to empty string field in nested object",
                "a": {"foo": {}},
                "b": {"foo":{"": 1}},
                "patch": [ {"op": "add", "path": "/foo/", "value":1 } ]
            },
            {
                "description": "add array to object",
                "a": {"foo": 1},
                "b": {"foo": 1, "bar": [1, 2]},
                "patch": [{"op": "add", "path": "/bar", "value": [1, 2]}]
            },
            {
                "description": "deep add field in list in object",
                "a": {"foo": 1, "baz": [{"qux": "hello"}]},
                "b": {"foo": 1, "baz": [{"qux": "hello", "foo": "world"}]},
                "patch": [{"op": "add", "path": "/baz/0/foo", "value": "world"}]
            },
            {
                "description": "add bool true",
                "a": {"foo": 1},
                "b": {"foo": 1, "bar": true},
                "patch": [{"op": "add", "path": "/bar", "value": true}]
            },
            {
                "description": "add bool false",
                "a": {"foo": 1},
                "b": {"foo": 1, "bar": false},
                "patch": [{"op": "add", "path": "/bar", "value": false}]
            },
            {
                "description": "add null",
                "a": {"foo": 1},
                "b": {"foo": 1, "bar": null},
                "patch": [{"op": "add", "path": "/bar", "value": null}]
            },
            {
                "description": "0 as field name",
                "a": {"foo": 1},
                "b": {"foo": 1, "0": "bar" },
                "patch": [{"op": "add", "path": "/0", "value": "bar"}]
            },
            {
                "description": "add to end of array",
                "a": ["foo"],
                "b": ["foo", "bar"],
                "patch": [{"op": "add", "path": "/1", "value": "bar"}]
            },
            {
                "description": "add to middle of array",
                "a": ["foo", "sil"],
                "b": ["foo", "bar", "sil"],
                "patch": [{"op": "add", "path": "/1", "value": "bar"}]
            },
            {
                "description": "add to beginning of array",
                "a": ["foo", "sil"],
                "b": ["bar", "foo", "sil"],
                "patch": [{"op": "add", "path": "/0", "value": "bar"}]
            },
            {
                "description": "add item to end of longer array",
                "a": ["foo", "sil"],
                "b": ["foo", "sil", "bar"],
                "patch": [{"op":"add", "path": "/2", "value": "bar"}]
            },
            {
                "description": "add nested array",
                "a": ["foo", "sil"],
                "b": ["foo", ["bar", "baz"], "sil"],
                "patch": [{"op": "add", "path": "/1", "value": ["bar", "baz"]}]
            },
            {
                "description": "add array as field value",
                "a": {"foo": 1, "bar": [1, 2, 3, 4]},
                "b": {"foo": 1},
                "patch": [{"op": "remove", "path": "/bar"}]
            },
            {
                "description": "deep remove string from object in array in object",
                "a": {"foo": 1, "baz": [{"qux": "hello"}]},
                "b": {"foo": 1, "baz": [{}]},
                "patch": [{"op": "remove", "path": "/baz/0/qux"}]
            },
            {
                "description": "deep replace array into object",
                "a": {"foo": 1, "baz": [{"qux": "hello"}]},
                "b": {"foo": [1, 2, 3, 4], "baz": [{"qux": "hello"}]},
                "patch": [{"op": "replace", "path": "/foo", "value": [1, 2, 3, 4]}]
            },
            {
                "description": "deep replace string into object in array in object",
                "a": {"foo": [1, 2, 3, 4], "baz": [{"qux": "hello"}]},
                "b": {"foo": [1, 2, 3, 4], "baz": [{"qux": "world"}]},
                "patch": [{"op": "replace", "path": "/baz/0/qux", "value": "world"}]
            },
            {
                "description": "replace string into array",
                "a": ["foo"],
                "b": ["bar"],
                "patch": [{"op": "replace", "path": "/0", "value": "bar"}]

            },
            {
                "description": "replace int into array",
                "a": [""],
                "b": [0],
                "patch": [{"op": "replace", "path": "/0", "value": 0}]
            },
            {
                "description": "replace bool true into array",
                "a": [""],
                "b": [true],
                "patch": [{"op": "replace", "path": "/0", "value": true}]
            },
            {
                "description": "replace bool false into array",
                "a": [""],
                "b": [false],
                "patch": [{"op": "replace", "path": "/0", "value": false}]
            },
            {
                "description": "replace null into array",
                "a": [""],
                "b": [null],
                "patch": [{"op": "replace", "path": "/0", "value": null}]
            },
            {
                "description": "replace to create nested array",
                "a": ["foo", "sil"],
                "b": ["foo", ["bar", "baz"]],
                "patch": [{"op": "replace", "path": "/1", "value": ["bar", "baz"]}]
            },
            {
                "description": "replace root",
                "a": {"foo": "bar"},
                "b": {"baz": "qux"},
                "patch": [{"op": "replace", "path": "", "value": {"baz": "qux"}}]
            },
            {
                "description": "replace null",
                "a": {"foo": null},
                "b": {"foo": "truthy"},
                "patch": [{"op": "replace", "path": "/foo", "value": "truthy"}]
            },
            {
                "description": "remove null",
                "a": {"foo": null},
                "b": {},
                "patch": [{"op": "remove", "path": "/foo"}]
            },
            {
                "description": "replace with null",
                "a": {"foo": "bar"},
                "b": {"foo": null},
                "patch": [{"op": "replace", "path": "/foo", "value": null}]
            },
            {
                "description": "removing first item",
                "a": [1, 2, 3, 4],
                "b": [2, 3, 4],
                "patch": [{"op": "remove", "path": "/0"}]
            },
            {
                "description": "removing multiple items",
                "a": [1, 2, 3, 4],
                "b": [1, 3],
                "patch": [
                    { "op": "remove", "path": "/3" },
                    { "op": "remove", "path": "/1" }
                ]
            },
            {
                "description": "complex list change",
                "a": [1, 2, 3, 4],
                "b": [1, 5, 4],
                "patch": [
                    { "op": "remove", "path": "/2" },
                    { "op": "replace", "path": "/1", "value": 5 }
                ]
            },
            {
                "description": "capitalisation",
                "a": {"foo":"bar"},
                "b": {"foo": "bar", "FOO": "BAR"},
                "patch": [{"op": "add", "path": "/FOO", "value": "BAR"}]
            },
            {
                "description": "giant replacements vs lots of small changes",
                "a": {"a": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, "add some length to this so it should not be replaced because repeating all this is more expensive than the overhead from the operations"]},
                "b": {"a": [11, 12, 13, 14, 5, 6, 7, 8, 9, 10, "add some length to this so it should not be replaced because repeating all this is more expensive than the overhead from the operations"]},
                "patch": [
                    { "op": "replace", "path": "/a/0", "value": 11 },
                    { "op": "replace", "path": "/a/1", "value": 12 },
                    { "op": "replace", "path": "/a/2", "value": 13 },
                    { "op": "replace", "path": "/a/3", "value": 14 }
                ]
            },
            {
                "description": "pick between lots of small changes vs a reasonable replacement",
                "a": {"a": { "b" : { "c" : { "1" : 1, "2": 2, "3": 3 } } } },
                "b": {"a": { "b" : { "c" : { "x" : 1, "y": 2, "z": 3 } } } },
                "patch": [{"op": "replace", "path": "/a/b/c", "value": { "x" : 1, "y": 2, "z": 3 }}]
            }
        ]
    """


testCaseDecoder : Json.Decoder TestCase
testCaseDecoder =
    Json.map5 TestCase
        (Json.field "description" Json.string)
        (Json.field "a" Json.value)
        (Json.field "b" Json.value)
        (Json.field "patch" Json.decoder)
        (Json.maybe (Json.field "skip" Json.bool) |> Json.map (Maybe.withDefault False))


testCases : Result Json.Error (List TestCase)
testCases =
    cases |> Json.decodeString (Json.list testCaseDecoder)
