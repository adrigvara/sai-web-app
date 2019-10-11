module Session exposing (Session, mutationReq, new, queryReq)

import Config
import Graphql.Http as Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder, Value)


type Session
    = Session String


new : Value -> Session
new flags =
    Session <| decodeApiUrl flags


decodeApiUrl : Value -> String
decodeApiUrl flags =
    case Decode.decodeValue apiUrlDecoder flags of
        Ok apiUrl ->
            apiUrl

        Err _ ->
            Config.apiUrl


apiUrlDecoder : Decoder String
apiUrlDecoder =
    Decode.field "apiUrl" Decode.string


queryReq :
    (Result (Http.Error decodesTo) decodesTo -> msg)
    -> SelectionSet decodesTo RootQuery
    -> Session
    -> Cmd msg
queryReq toMsg query (Session apiUrl) =
    query
        |> Http.queryRequest apiUrl
        |> Http.send toMsg


mutationReq :
    (Result (Http.Error decodesTo) decodesTo -> msg)
    -> SelectionSet decodesTo RootMutation
    -> Session
    -> Cmd msg
mutationReq toMsg mutation (Session apiUrl) =
    mutation
        |> Http.mutationRequest apiUrl
        |> Http.send toMsg
