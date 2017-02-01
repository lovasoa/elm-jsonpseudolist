module JsonPseudoList exposing (jsonPseudoList)

{-|
Elm Json.Decoder for javascript Array-like objects, of the form:

    {length:2, 0:value0, 1:value1}

@docs jsonPseudoList
-}

import Json.Decode exposing (..)


{-| Read an object of the form

    >>> Json.Decode.decodeString
    ...     (jsonPseudoList Json.Decode.string)
    ...     """{
    ...           "length": 2,
    ...           "0": "hello"
    ...           "1": "world"
    ...         }"""
    ["hello", "world"]
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
