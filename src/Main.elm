module Main exposing (main)

import Browser
import Device
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
import Grid
import Html exposing (Html)
import Json.Decode as Decode exposing (Value)
import SAI.Object
import SAI.Object.Person as Person
import SAI.Query as Query
import SAI.ScalarCodecs exposing (Id)
import Session exposing (Session)



-- MAIN


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \model -> view model.device model.page
        , subscriptions = subs
        }



-- MODEL


type alias Model =
    { device : Device
    , session : Session
    , page : People
    }


type People
    = Loading
    | Loaded (List Person)
    | NotLoaded (Http.Error (List Person))


type alias Person =
    { id : Id
    , image : String
    , name : String
    , email : Maybe String
    , phone : Maybe String
    , address : Maybe String
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        session =
            Session.fromFlags flags
    in
    ( Model (Device.fromFlags flags) session Loading, Cmd.batch [ getPeople session, Device.get GotDevice ] )



-- UPDATE


type Msg
    = GotPeople (List Person)
    | NotGotPeople (Http.Error (List Person))
    | GotDevice Device


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        GotPeople personList ->
            { model | page = Loaded personList }

        NotGotPeople error ->
            { model | page = NotLoaded error }

        GotDevice device ->
            { model | device = device }


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
    SelectionSet.map6 Person
        Person.id
        Person.image
        Person.fullName
        Person.email
        Person.phone
        Person.address



-- VIEW


view : Device -> People -> Browser.Document Msg
view device people =
    { title = "People | SAI"
    , body =
        [ layout
            [ Background.color <| rgb255 235 237 239
            ]
          <|
            pageView device people
        ]
    }


type alias View =
    Element Msg


type alias Rule =
    Attribute Msg


type alias Rules =
    List Rule


pageView : Device -> People -> Element Msg
pageView { class } page =
    case ( page, class ) of
        ( Loading, _ ) ->
            text "Loading..."

        ( Loaded personList, Phone ) ->
            peopleView 1 personList

        ( Loaded personList, Tablet ) ->
            peopleView 2 personList

        ( Loaded personList, Desktop ) ->
            peopleView 3 personList

        ( Loaded personList, BigDesktop ) ->
            peopleView 4 personList

        ( NotLoaded error, _ ) ->
            column [] <| errorView error


peopleView : Int -> List Person -> View
peopleView peoplePerRow people =
    Grid.grid
        [ Grid.spacing 16
        , Grid.padding 16
        , Grid.emptyPadding personPadding
        , Grid.elementsPerRow peoplePerRow
        ]
        (List.map personView people)


personPadding : Int
personPadding =
    16


personView : Person -> Element Msg
personView person =
    column
        [ padding personPadding
        , spacing 16
        , width fill
        , height fill
        , Background.color <| rgb255 255 255 255
        ]
        [ imageView person.image
        , nameView person.name
        , contactInfoView person
        ]


imageView : String -> Element Msg
imageView src =
    image imageAttributes { src = src, description = "" }


imageAttributes : List (Attribute msg)
imageAttributes =
    [ centerX, width (fill |> maximum 150 |> minimum 50), height (fill |> maximum 150 |> minimum 50) ]


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


errorView : Http.Error error -> List (Element Msg)
errorView error =
    case error of
        Http.GraphqlError _ graphqlErrors ->
            List.map (.message >> text) graphqlErrors

        Http.HttpError httpError ->
            [ httpError |> httpErrorString |> text ]


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


subs : Model -> Sub Msg
subs _ =
    Sub.batch [ Device.sub GotDevice ]
