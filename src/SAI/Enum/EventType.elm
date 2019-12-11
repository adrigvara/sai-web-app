-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SAI.Enum.EventType exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-|

  - Independent -
  - Dependent -

-}
type EventType
    = Independent
    | Dependent


list : List EventType
list =
    [ Independent, Dependent ]


decoder : Decoder EventType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "INDEPENDENT" ->
                        Decode.succeed Independent

                    "DEPENDENT" ->
                        Decode.succeed Dependent

                    _ ->
                        Decode.fail ("Invalid EventType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : EventType -> String
toString enum =
    case enum of
        Independent ->
            "INDEPENDENT"

        Dependent ->
            "DEPENDENT"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe EventType
fromString enumString =
    case enumString of
        "INDEPENDENT" ->
            Just Independent

        "DEPENDENT" ->
            Just Dependent

        _ ->
            Nothing