module Main exposing (main)

import Browser
import Browser.Events
import Config
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
import Html exposing (Html)
import Json.Decode as Decode exposing (Value)
import List.Extra
import SAI.Object
import SAI.Object.Person as Person
import SAI.Query as Query
import SAI.ScalarCodecs exposing (Id)
import Session exposing (Session)
import Window



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }



-- MODEL


type Model
    = Model Device Session People


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
    case ( msg, model ) of
        ( GotPeople personList, Model device session _ ) ->
            Model device session (Loaded personList)

        ( NotGotPeople error, Model device session _ ) ->
            Model device session (NotLoaded error)

        ( GotDevice device, Model _ session people ) ->
            Model device session people


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
        imageSel
        Person.fullName
        Person.email
        Person.phone
        Person.address


imageSel : SelectionSet String SAI.Object.Person
imageSel =
    SelectionSet.map (Maybe.withDefault "https://img.myloview.es/fotomurales/icono-de-persona-generica-400-114079057.jpg") Person.image



-- VIEW


view : Model -> Browser.Document Msg
view (Model device _ people) =
    { title = "People | SAI"
    , body =
        [ layout
            [ Background.color <| rgb255 235 237 239
            ]
          <|
            pageView device people
        ]
    }


pageView : Device -> People -> Element Msg
pageView { class, orientation } people =
    case ( people, class, orientation ) of
        ( Loading, _, _ ) ->
            text "Loading..."

        ( Loaded personList, Phone, Portrait ) ->
            peopleView 1 personList

        ( Loaded personList, Phone, Landscape ) ->
            peopleView 2 personList

        ( Loaded personList, Tablet, Portrait ) ->
            peopleView 2 personList

        ( Loaded personList, Tablet, Landscape ) ->
            peopleView 3 personList

        ( Loaded personList, Desktop, Portrait ) ->
            peopleView 3 personList

        ( Loaded personList, Desktop, Landscape ) ->
            peopleView 4 personList

        ( Loaded personList, BigDesktop, Portrait ) ->
            peopleView 4 personList

        ( Loaded personList, BigDesktop, Landscape ) ->
            peopleView 5 personList

        ( NotLoaded error, _, _ ) ->
            column [] <| errorView error


peopleView : Int -> List Person -> Element Msg
peopleView peoplePerRow personList =
    column
        [ padding 32
        , spacing 16
        , width fill
        , height fill
        ]
    <|
        personViewsRows peoplePerRow personList


personPadding : Int
personPadding =
    16


personViewsRows : Int -> List Person -> List (Element Msg)
personViewsRows peoplePerRow personList =
    personList
        |> List.map personView
        |> List.Extra.greedyGroupsOf peoplePerRow
        |> fillLast { desiredNumber = peoplePerRow, emptyPadding = personPadding }
        |> List.map (row [ spacing 16, width fill ])


fillLast :
    { desiredNumber : Int, emptyPadding : Int }
    -> List (List (Element msg))
    -> List (List (Element msg))
fillLast opts table =
    List.reverse <|
        case List.reverse table of
            x :: xs ->
                fillWithEmptys opts x :: xs

            x ->
                x


fillWithEmptys :
    { desiredNumber : Int, emptyPadding : Int }
    -> List (Element msg)
    -> List (Element msg)
fillWithEmptys { desiredNumber, emptyPadding } list =
    list
        ++ List.repeat
            (desiredNumber - List.length list)
            (emptyElement emptyPadding)


emptyElement : Int -> Element msg
emptyElement emptyPadding =
    el [ width fill, height fill, padding emptyPadding ] none


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
subs (Model _ _ _) =
    Sub.batch [ Device.sub GotDevice ]
