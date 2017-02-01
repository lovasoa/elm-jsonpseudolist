module JsonPseudoList exposing (jsonPseudoList)

{-|
Elm Json.Decoder for javascript Array-like objects, of the form:

    {length:2, 0:value0, 1:value1}

@docs jsonPseudoList
-}

import Json.Decode exposing (..)


{-| Create a decoder for objects of the form:

    {"length": Number, "0": Value, "1": Value, ...}

    Takes just one argument: the decoder for `Value`

    You need to use Json.Decode with this module
    >>> import Json.Decode

    >>> Json.Decode.decodeString
    ...     (jsonPseudoList Json.Decode.string)
    ...     """{
    ...           "length": 2,
    ...           "0": "hello",
    ...           "1": "world"
    ...         }"""
    Ok ["hello", "world"]
-}
jsonPseudoList : Decoder a -> Decoder (List a)
jsonPseudoList decoder =
    field "length" int
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
