module Json.Diff exposing (diff, invertibleDiff, diffWithCustomWeight)

{-| Create a JSON patch by inspecting the difference between two JSON values.

This has been implemented rather simply, and is probably not very optimised, but it should work for a lot of use cases.

@docs diff, invertibleDiff, diffWithCustomWeight

-}

import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode as JsonE
import Json.Patch as JsonP
import Json.Patch.Invertible as Invertible
import Json.Pointer as Json
import Set


{-| Create a [`Patch`](https://package.elm-lang.org/packages/norpan/elm-json-patch/latest/Json-Patch#Patch) that when
applied to the first value will produce the second value.

This uses a relatively expensive, but not perfect weight function to decide what patch is best (it basically encodes
the JSON to a string and looks at the length), but it should give pretty reasonable results most of the time. If you
want something more efficient or accurate you can use [`diffWithCustomWeight`](Json.Diff#diffWithCustomWeight).

Note that this is a simple diff that never produces moves or copies, and as such the patches will be inefficient if
those are common operations in your case.

-}
diff : Json.Value -> Json.Value -> JsonP.Patch
diff a b =
    internalDiff defaultPatchWeight [] a b |> Invertible.toMinimalPatch


{-| Create a patch just as with [`diff`](Json.Diff#diff), but in the form of an
[`Invertible.Patch`](Json.Patch.Invertible#Patch) which contains all the extra information required to invert the patch
to return a document to its original form.
-}
invertibleDiff : Json.Value -> Json.Value -> Invertible.Patch
invertibleDiff a b =
    internalDiff defaultPatchWeight [] a b


{-| Do a diff with a custom weight. This returns an invertible patch, if you want a normal one use
[`Invertible.toMinimalPatch`](Json.Patch.Invertible#toMinimalPatch) on the result.

The default weight function is
`patch |> Invertible.toMinimalPatch |> Json.Patch.encoder |> Json.Encode.encode 0 |> String.length` which is reasonably
expensive.

The cheapest reasonable function you might want to use is
[`List.length`](https://package.elm-lang.org/packages/elm/core/latest/List#length), which is generally OK, but will
produce poor results if replacements involve large chunks of JSON.

If you always use invertible patches, then using [`Invertible.toPatch`](Json.Patch.Invertible#toPatch) in the weight
function over [`Invertible.toMinimalPatch`](Json.Patch.Invertible#toMinimalPatch) would provide a more accurate weight.

-}
diffWithCustomWeight : (Invertible.Patch -> Int) -> Json.Value -> Json.Value -> Invertible.Patch
diffWithCustomWeight weight =
    internalDiff weight []



{- Private -}


internalDiff : (Invertible.Patch -> Int) -> Json.Pointer -> Json.Value -> Json.Value -> Invertible.Patch
internalDiff weight root a b =
    let
        replace =
            [ Invertible.Replace root a b ]
    in
    case try primitiveDecoder a b |> Maybe.map primitiveEquals of
        Just equal ->
            if equal then
                []

            else
                replace

        Nothing ->
            case try (Json.list Json.value) a b |> Maybe.map (diffList weight root) of
                Just d ->
                    d

                Nothing ->
                    case try (Json.dict Json.value) a b |> Maybe.andThen (diffObject weight root) of
                        Just modify ->
                            if weight modify > weight replace then
                                replace

                            else
                                modify

                        Nothing ->
                            replace


defaultPatchWeight : Invertible.Patch -> Int
defaultPatchWeight patch =
    patch |> Invertible.toMinimalPatch |> JsonP.encoder |> jsonWeight


jsonWeight : Json.Value -> Int
jsonWeight value =
    value |> JsonE.encode 0 |> String.length


primitiveDecoder : Json.Decoder Primitive
primitiveDecoder =
    Json.oneOf
        [ Json.string |> Json.map S
        , Json.bool |> Json.map B
        , Json.int |> Json.map I
        , Json.float |> Json.map F
        , Json.null N
        ]


type Primitive
    = S String
    | B Bool
    | I Int
    | F Float
    | N


primitiveEquals : ( Primitive, Primitive ) -> Bool
primitiveEquals ( a, b ) =
    case a of
        S aString ->
            case b of
                S bString ->
                    aString == bString

                _ ->
                    False

        B aBool ->
            case b of
                B bBool ->
                    aBool == bBool

                _ ->
                    False

        I aInt ->
            case b of
                I bInt ->
                    aInt == bInt

                _ ->
                    False

        F aFloat ->
            case b of
                F bFloat ->
                    aFloat == bFloat

                _ ->
                    False

        N ->
            case b of
                N ->
                    True

                _ ->
                    False


try : Json.Decoder v -> Json.Value -> Json.Value -> Maybe ( v, v )
try decoder a b =
    Maybe.map2
        (\ja -> \jb -> ( ja, jb ))
        (a |> Json.decodeValue decoder |> Result.toMaybe)
        (b |> Json.decodeValue decoder |> Result.toMaybe)


diffList : (Invertible.Patch -> Int) -> Json.Pointer -> ( List Json.Value, List Json.Value ) -> Invertible.Patch
diffList weight root ( a, b ) =
    let
        ( operations, _ ) =
            ops weight root (List.reverse a) (List.reverse b)
    in
    operations
        |> List.reverse
        |> List.sortBy (\( _, p ) -> p |> Maybe.map (\pv -> -pv) |> Maybe.withDefault 0)
        |> List.map (\( op, _ ) -> op)


diffObject : (Invertible.Patch -> Int) -> Json.Pointer -> ( Dict String Json.Value, Dict String Json.Value ) -> Maybe Invertible.Patch
diffObject weight root ( a, b ) =
    let
        aKeys =
            a |> Dict.keys |> Set.fromList

        emptyA =
            aKeys |> Set.isEmpty

        bKeys =
            b |> Dict.keys |> Set.fromList

        emptyB =
            bKeys |> Set.isEmpty
    in
    if emptyA && emptyB then
        Just []

    else
        Set.union aKeys bKeys
            |> Set.toList
            |> List.concatMap (\k -> diffField weight root k (Dict.get k a) (Dict.get k b))
            |> Just


diffField : (Invertible.Patch -> Int) -> Json.Pointer -> String -> Maybe Json.Value -> Maybe Json.Value -> Invertible.Patch
diffField weight root key a b =
    let
        pointer =
            root ++ [ key ]
    in
    case a of
        Just ja ->
            case b of
                Just jb ->
                    internalDiff weight pointer ja jb

                Nothing ->
                    [ Invertible.Remove pointer ja ]

        Nothing ->
            case b of
                Just jb ->
                    [ Invertible.Add pointer jb ]

                Nothing ->
                    []


ops : (Invertible.Patch -> Int) -> Json.Pointer -> List Json.Value -> List Json.Value -> ( List ( Invertible.Operation, Maybe Int ), Int )
ops weight root a b =
    case ( a, b ) of
        ( [], [] ) ->
            ( [], 0 )

        ( h :: t, [] ) ->
            let
                ( operations, w ) =
                    ops weight root t []

                index =
                    List.length t

                op =
                    Invertible.Remove (root ++ [ List.length t |> String.fromInt ]) h
            in
            ( ( op, Just index ) :: operations, w + weight [ op ] )

        ( [], h :: t ) ->
            let
                ( operations, w ) =
                    ops weight root [] t

                op =
                    Invertible.Add (root ++ [ List.length t |> String.fromInt ]) h
            in
            ( ( op, Nothing ) :: operations, w + weight [ op ] )

        ( ah :: at, bh :: bt ) ->
            let
                differences =
                    internalDiff weight (root ++ [ List.length bt |> String.fromInt ]) ah bh
            in
            case differences of
                [] ->
                    ops weight root at bt

                _ ->
                    let
                        results =
                            [ ( ops weight root at (bh :: bt)
                              , [ ( Invertible.Remove (root ++ [ List.length at |> String.fromInt ]) ah, List.length at |> Just ) ]
                              )
                            , ( ops weight root (ah :: at) bt
                              , [ ( Invertible.Add (root ++ [ List.length bt |> String.fromInt ]) bh, Nothing ) ]
                              )
                            , ( ops weight root at bt
                              , differences |> List.map (\op -> ( op, Nothing ))
                              )
                            ]

                        getWeight ( _, w ) =
                            w

                        combine ( ( result, w ), operations ) =
                            ( operations ++ result, w + weight (operations |> List.map (\( op, _ ) -> op)) )
                    in
                    results
                        |> List.map combine
                        |> List.sortBy getWeight
                        |> List.head
                        |> Maybe.withDefault ( [], 0 )
