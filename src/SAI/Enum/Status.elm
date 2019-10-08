-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SAI.Enum.Status exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-|

  - Active -
  - Inactive -

-}
type Status
    = Active
    | Inactive


list : List Status
list =
    [ Active, Inactive ]


decoder : Decoder Status
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ACTIVE" ->
                        Decode.succeed Active

                    "INACTIVE" ->
                        Decode.succeed Inactive

                    _ ->
                        Decode.fail ("Invalid Status type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Status -> String
toString enum =
    case enum of
        Active ->
            "ACTIVE"

        Inactive ->
            "INACTIVE"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Status
fromString enumString =
    case enumString of
        "ACTIVE" ->
            Just Active

        "INACTIVE" ->
            Just Inactive

        _ ->
            Nothing