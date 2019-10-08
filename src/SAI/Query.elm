-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SAI.Query exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import SAI.InputObject
import SAI.Interface
import SAI.Object
import SAI.Scalar
import SAI.ScalarCodecs
import SAI.Union


{-| -}
people : SelectionSet decodesTo SAI.Object.Person -> SelectionSet (List decodesTo) RootQuery
people object_ =
    Object.selectionForCompositeField "people" [] object_ (identity >> Decode.list)


type alias PersonRequiredArguments =
    { id : SAI.ScalarCodecs.Id }


{-|

  - id -

-}
person : PersonRequiredArguments -> SelectionSet decodesTo SAI.Object.Person -> SelectionSet (Maybe decodesTo) RootQuery
person requiredArgs object_ =
    Object.selectionForCompositeField "person" [ Argument.required "id" requiredArgs.id (SAI.ScalarCodecs.codecs |> SAI.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


{-| -}
groups : SelectionSet decodesTo SAI.Object.Group -> SelectionSet (List decodesTo) RootQuery
groups object_ =
    Object.selectionForCompositeField "groups" [] object_ (identity >> Decode.list)


type alias GroupRequiredArguments =
    { id : SAI.ScalarCodecs.Id }


{-|

  - id -

-}
group : GroupRequiredArguments -> SelectionSet decodesTo SAI.Object.Group -> SelectionSet (Maybe decodesTo) RootQuery
group requiredArgs object_ =
    Object.selectionForCompositeField "group" [ Argument.required "id" requiredArgs.id (SAI.ScalarCodecs.codecs |> SAI.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


{-| -}
users : SelectionSet decodesTo SAI.Object.User -> SelectionSet (List decodesTo) RootQuery
users object_ =
    Object.selectionForCompositeField "users" [] object_ (identity >> Decode.list)


type alias UserRequiredArguments =
    { id : SAI.ScalarCodecs.Id }


{-|

  - id -

-}
user : UserRequiredArguments -> SelectionSet decodesTo SAI.Object.User -> SelectionSet (Maybe decodesTo) RootQuery
user requiredArgs object_ =
    Object.selectionForCompositeField "user" [ Argument.required "id" requiredArgs.id (SAI.ScalarCodecs.codecs |> SAI.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


type alias LoginRequiredArguments =
    { username : String
    , password : String
    }


{-|

  - username -
  - password -

-}
login : LoginRequiredArguments -> SelectionSet decodesTo SAI.Object.LoginResult -> SelectionSet decodesTo RootQuery
login requiredArgs object_ =
    Object.selectionForCompositeField "login" [ Argument.required "username" requiredArgs.username Encode.string, Argument.required "password" requiredArgs.password Encode.string ] object_ identity
