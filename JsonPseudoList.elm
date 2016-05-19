module JsonPseudoList exposing (jsonPseudoList)

{-|
Elm Json.Decoder for javascript Array-like objects, of the form:

    {length:2, 0:value0, 1:value1}

@docs jsonPseudoList
-}

{-| Read an object of the form
{
  "length" : 2
  "0" : Object0
  "1" : Object1
}
-}
jsonPseudoList : Json.Decoder a -> Json.Decoder (List a)
jsonPseudoList decoder =
  let
    addvalue num list =
      Json.map (\val -> val::list) ((toString num) := decoder)
    fromLength length =
      List.foldr
        (\num prevdec -> prevdec `Json.andThen` (addvalue num))
        (Json.succeed [])
        [0..length-1]
  in
  ("length" := Json.int) `Json.andThen` fromLength