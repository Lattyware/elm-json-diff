# elm-json-diff

[![Elm package](https://img.shields.io/elm-package/v/lattyware/elm-json-diff)](https://package.elm-lang.org/packages/lattyware/elm-json-diff/latest/) [![GitHub](https://img.shields.io/github/license/lattyware/elm-json-diff)](https://github.com/Lattyware/elm-json-diff/blob/1.0.0/LICENSE) [![GitHub Workflow Status](https://img.shields.io/github/workflow/status/lattyware/elm-json-diff/Tests)](https://github.com/Lattyware/elm-json-diff/actions?query=workflow%3ATests)

Compute JSON patches by comparing two JSON values.

## Usage

    patch =
        Json.Diff.diff oldDocument newDocument
        
Creating a patch has to be done on the `Value` type, due to the type system (records can't be accessed by field name in 
Elm).

However, if you have an encoder for your Elm type, you can create a patch by encoding the data first.
