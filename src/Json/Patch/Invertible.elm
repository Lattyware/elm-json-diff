module Json.Patch.Invertible exposing
    ( Patch, Operation(..)
    , invert, apply
    , toPatch, toMinimalPatch, fromPatch
    )

{-| JSON patches, when structured a specific way, can be inverted to return a patched document to its original form.


# Types

@docs Patch, Operation


# Operations

@docs invert, apply


# Compatibility

@docs toPatch, toMinimalPatch, fromPatch

-}

import Json.Decode as Json
import Json.Patch as Json
import Json.Pointer as Json


{-| A list of invertible [`Operation`](Json.Patch.Invertible#Operation])s that are performed one after another.
-}
type alias Patch =
    List Operation


{-| An operation that can be inverted to produce a reverse patch.

This is largely meant to mirror
[`Json.Patch.Operation`](https://package.elm-lang.org/packages/norpan/elm-json-patch/latest/Json-Patch#Operation), but
structured with any additional information required to invert the patch.

However, there are some key differences: `Test` operations are not allowed, as they are used to include the extra
information. `Copy` operations are not allowed, as they are ambiguous (the reverse of a `Copy` is a `Remove`, which
could then become either an `Add` or a `Copy`).

-}
type Operation
    = Add Json.Pointer Json.Value
    | Remove Json.Pointer Json.Value
    | Replace Json.Pointer Json.Value Json.Value
    | Move Json.Pointer Json.Pointer


{-| Create a patch that does the inverse of the given one.

`patch |> invert |> invert` should always result in the same thing as just `patch`, and
`value |> (apply patch) |> Result.andThen (apply (patch |> invert))` should produce `value` (assuming the patch can
be applied to the value).

-}
invert : Patch -> Patch
invert =
    List.reverse >> List.map invertOp


{-| Apply a [`Patch`](Json.Patch.Invertible#Patch) to a
[`Value`](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#Value).

This is just a convenience function to do `patch |> toMinimalPatch |> Json.Patch.apply`.

-}
apply : Patch -> Json.Value -> Result String Json.Value
apply =
    toMinimalPatch >> Json.apply


{-| Create a normal patch from an invertible one.

Note that this patch would still be invertible, you are just losing the type guarantee of that (i.e: you could run
[`fromPatch`](Json.Patch.Invertible#fromPatch) on it and get a result). To strip the tests and make the patch truly not
invertible, see [`toMinimalPatch`](Json.Patch.Invertible#toMinimalPatch) instead.

-}
toPatch : Patch -> Json.Patch
toPatch =
    List.concatMap toOperations


{-| Create a patch from the invertible one, stripping out the tests that allow inversion.
-}
toMinimalPatch : Patch -> Json.Patch
toMinimalPatch =
    List.concatMap toMinimalOperations


{-| Try to create an invertible patch from a normal one. This will only work if the normal patch is structured
correctly (with tests before operations to establish the "missing" values).

[`Diff.invertibleDiff`](Json.Diff#invertibleDiff) can provide one of these with no chance of failure.

-}
fromPatch : Json.Patch -> Result String Patch
fromPatch patch =
    case patch of
        (Json.Add pointer value) :: rest ->
            fromPatch rest |> Result.map ((::) (Add pointer value))

        (Json.Test testPointer value) :: (Json.Remove pointer) :: rest ->
            if testPointer == pointer then
                fromPatch rest |> Result.map ((::) (Remove pointer value))

            else
                Err "The test before a remove must refer to the same pointer to be invertible."

        (Json.Remove _) :: _ ->
            Err "Remove operations must be preceded by a test for the old value to be invertible."

        (Json.Test testPointer old) :: (Json.Replace pointer new) :: rest ->
            if testPointer == pointer then
                fromPatch rest |> Result.map ((::) (Replace pointer old new))

            else
                Err "The test before a replace must refer to the same pointer to be invertible."

        (Json.Replace _ _) :: _ ->
            Err "Replace operations must be preceded by a test for the old value to be invertible."

        (Json.Move old new) :: rest ->
            fromPatch rest |> Result.map ((::) (Move old new))

        (Json.Copy _ _) :: _ ->
            Err "Copy operations are ambiguous."

        (Json.Test _ _) :: _ ->
            Err "Standalone test operations are not supported."

        [] ->
            Ok []



{- Private -}


invertOp : Operation -> Operation
invertOp op =
    case op of
        Add pointer value ->
            Remove pointer value

        Remove pointer value ->
            Add pointer value

        Replace pointer old new ->
            Replace pointer new old

        Move old new ->
            Move new old


toOperations : Operation -> List Json.Operation
toOperations op =
    case op of
        Add pointer value ->
            [ Json.Add pointer value ]

        Remove pointer value ->
            [ Json.Test pointer value, Json.Remove pointer ]

        Replace pointer old new ->
            [ Json.Test pointer old, Json.Replace pointer new ]

        Move old new ->
            [ Json.Move old new ]


toMinimalOperations : Operation -> List Json.Operation
toMinimalOperations op =
    case op of
        Add pointer value ->
            [ Json.Add pointer value ]

        Remove pointer _ ->
            [ Json.Remove pointer ]

        Replace pointer _ new ->
            [ Json.Replace pointer new ]

        Move old new ->
            [ Json.Move old new ]
