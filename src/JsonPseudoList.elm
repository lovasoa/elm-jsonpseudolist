module JsonPseudoList exposing (jsonPseudoList)

{-|
Elm Json.Decoder for javascript Array-like objects, of the form:

    {length:2, 0:value0, 1:value1}

This is usefull for decoding things like
[DOM `NodeList`s](https://developer.mozilla.org/en/docs/Web/API/NodeList)
coming from javascript ports.

@docs jsonPseudoList
-}

import Json.Decode exposing (..)


{-| Create a decoder for objects of the form:
`{"length": Number, "0": Value, "1": Value, ...}`

    Takes just one argument: the decoder for `Value`

    You need to use Json.Decode with this module
    >>> import Json.Decode

### Decode an Array-like object with two values

    >>> Json.Decode.decodeString
    ...     (jsonPseudoList Json.Decode.string)
    ...     """{
    ...           "length": 2,
    ...           "0": "hello",
    ...           "1": "world"
    ...         }"""
    Ok ["hello", "world"]

### Try to decode an invalid Array-like object
The object to decode may contain supplumentary fields, but
if a value is missing, the decoder will fail

    >>> Json.Decode.decodeString
    ...     (jsonPseudoList Json.Decode.string)
    ...     """{
    ...           "length": 2,
    ...           "0": "hello"
    ...         }"""
    Err "Expecting an object with a field named `1` but instead got: {\"0\":\"hello\",\"length\":2}"

### Fails on negative lengths
Of course, if the length is less then 0, decoding fails

    >>> Json.Decode.decodeString
    ...     (jsonPseudoList Json.Decode.string)
    ...     """{
    ...           "length": -1
    ...         }"""
    Err "I ran into a `fail` decoder: Expecting a positive value for the `length` field"
-}
jsonPseudoList : Decoder a -> Decoder (List a)
jsonPseudoList decoder =
    field "length" int
        |> andThen
            (\n ->
                if n < 0 then
                    fail "Expecting a positive value for the `length` field"
                else
                    succeed n
            )
        |> andThen (fromLength decoder)


fromLength : Decoder a -> Int -> Decoder (List a)
fromLength decoder length =
    List.foldr
        (\num -> andThen (addvalue decoder num))
        (succeed [])
        (List.range 0 (length - 1))


addvalue : Decoder a -> Int -> List a -> Decoder (List a)
addvalue decoder num list =
    map
        (\val -> val :: list)
        (field (toString num) decoder)
