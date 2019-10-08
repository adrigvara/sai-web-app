module Main exposing (main)

import Browser
import Graphql.Http as Http
import Graphql.Http.GraphqlError as GraphqlError exposing (GraphqlError)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Json.Decode as Decode exposing (Value)
import SAI.Object
import SAI.Object.Person as Person
import SAI.Query as Query
import Session exposing (Session)
import Spinner



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Model People Session


type People
    = Loading Spinner.Model
    | Loaded (List Person)
    | NotLoaded (Http.Error (List Person))


type alias Person =
    { name : String
    , email : Maybe String
    , phone : Maybe String
    , address : Maybe String
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        session =
            Session.new flags
    in
    ( Model (Loading Spinner.init) session, getPeople session )


getPeople : Session -> Cmd Msg
getPeople =
    Session.queryReq peopleResultToMsg peopleSel


peopleResultToMsg : Result (Http.Error (List Person)) (List Person) -> Msg
peopleResultToMsg peopeResult =
    case peopeResult of
        Ok personList ->
            GotPeople personList

        Err peopleError ->
            NotGotPeople peopleError


peopleSel : SelectionSet (List Person) RootQuery
peopleSel =
    Query.people personSel


personSel : SelectionSet Person SAI.Object.Person
personSel =
    SelectionSet.map4 Person
        Person.name
        Person.email
        Person.phone
        Person.address



-- UPDATE


type Msg
    = GotPeople (List Person)
    | NotGotPeople (Http.Error (List Person))
    | GotSpinnerMsg Spinner.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case ( msg, model ) of
        ( GotPeople personList, Model _ apiUrl ) ->
            Model (Loaded personList) apiUrl

        ( NotGotPeople error, Model _ apiUrl ) ->
            Model (NotLoaded error) apiUrl

        ( GotSpinnerMsg subMsg, Model (Loading spinner) apiUrl ) ->
            Model (Loading (Spinner.update subMsg spinner)) apiUrl

        ( GotSpinnerMsg _, _ ) ->
            model



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "People | SAI"
    , body = bodyView model
    }


bodyView : Model -> List (Html Msg)
bodyView (Model people _) =
    case people of
        Loading spinner ->
            [ spinnerView spinner ]

        Loaded personList ->
            List.map personView personList

        NotLoaded error ->
            errorView error


spinnerView : Spinner.Model -> Html Msg
spinnerView spinner =
    Html.div [] [ Spinner.view Spinner.defaultConfig spinner ]


personView : Person -> Html Msg
personView person =
    Html.div [] [ Html.text (.name person) ]


errorView : Http.Error x -> List (Html Msg)
errorView error =
    case error of
        Http.GraphqlError _ graphqlErrors ->
            List.map graphqlErrorView graphqlErrors

        Http.HttpError httpError ->
            [ httpErrorView httpError ]


graphqlErrorView : GraphqlError -> Html Msg
graphqlErrorView =
    .message >> stringErrorView


httpErrorView : Http.HttpError -> Html Msg
httpErrorView =
    httpErrorString >> stringErrorView


stringErrorView : String -> Html Msg
stringErrorView =
    Html.text


httpErrorString : Http.HttpError -> String
httpErrorString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus resp bodyString ->
            parseError bodyString
                |> Maybe.withDefault (String.fromInt resp.statusCode ++ " " ++ resp.statusText)

        Http.BadPayload text ->
            "Unexpected response from api: " ++ Decode.errorToString text

        Http.BadUrl url ->
            "Malformed url: " ++ url


parseError : String -> Maybe String
parseError =
    Decode.decodeString (Decode.field "error" Decode.string) >> Result.toMaybe



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Model (Loading _) _ ->
            Sub.map GotSpinnerMsg Spinner.subscription

        _ ->
            Sub.none
