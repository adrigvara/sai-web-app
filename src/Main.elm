module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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
    = Model Session People


type People
    = Loading
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
    ( Model session Loading, getPeople session )



-- UPDATE


type Msg
    = GotPeople (List Person)
    | NotGotPeople (Http.Error (List Person))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case ( msg, model ) of
        ( GotPeople personList, Model session _ ) ->
            Model session (Loaded personList)

        ( NotGotPeople error, Model session _ ) ->
            Model session (NotLoaded error)


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
        Person.fullName
        Person.email
        Person.phone
        Person.address



-- VIEW


view : Model -> Browser.Document Msg
view (Model _ people) =
    { title = "People | SAI"
    , body = [ peopleView people ]
    }


peopleView : People -> Html Msg
peopleView people =
    case people of
        Loading ->
            layout [ Background.color <| rgb255 235 237 239 ] <|
                text "Loading..."

        Loaded personList ->
            layout [ Background.color <| rgb255 235 237 239 ] <|
                wrappedRow [ padding 16, spacing 16 ] <|
                    List.map personView personList

        NotLoaded error ->
            Html.div [] <| errorView error


personView : Person -> Element Msg
personView person =
    column
        [ padding 16
        , spacing 16
        , width <| px 320
        , height fill
        , centerX
        , Background.color <| rgb255 255 255 255
        ]
        [ nameView person.name
        , contactInfoView person
        ]


nameView : String -> Element Msg
nameView name =
    el [ Font.size 15 ] <| text name


contactInfoView : Person -> Element Msg
contactInfoView person =
    column [ Font.size 12, spacing 5 ]
        [ phoneView person.phone
        , emailView person.email
        , addressView person.address
        ]


phoneView : Maybe String -> Element Msg
phoneView phone =
    case phone of
        Just phoneString ->
            el [] <| text phoneString

        Nothing ->
            none


emailView : Maybe String -> Element Msg
emailView email =
    case email of
        Just emailString ->
            el [] <| text emailString

        Nothing ->
            none


addressView : Maybe String -> Element Msg
addressView address =
    case address of
        Just addressString ->
            el [] <| text addressString

        Nothing ->
            none


errorView : Http.Error x -> List (Html Msg)
errorView error =
    case error of
        Http.GraphqlError _ graphqlErrors ->
            List.map (.message >> Html.text) graphqlErrors

        Http.HttpError httpError ->
            [ httpError |> httpErrorString |> Html.text ]


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
subscriptions (Model _ _) =
    Sub.none
