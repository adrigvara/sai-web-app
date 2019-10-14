module Spinner exposing (Attribute(..), rotation, threeCircles)

import Element exposing (..)
import Framework.Color as Color
import Framework.Spinner as Spinner exposing (..)
import List.Extra


type Attribute
    = Color Element.Color
    | Size Int
    | Center
    | CenterX
    | CenterY


threeCircles : List Attribute -> Element msg
threeCircles =
    getSpinner ThreeCircles


rotation : List Attribute -> Element msg
rotation =
    getSpinner Rotation


getSpinner : Spinner -> List Attribute -> Element msg
getSpinner spinner attributes =
    el
        (getAttributes attributes)
        (Spinner.spinner spinner (getSize attributes) (getColor attributes))


getAttributes : List Attribute -> List (Element.Attribute msg)
getAttributes attributes =
    getCenterAttributes attributes ++ List.filterMap getAttribute attributes


getCenterAttributes : List Attribute -> List (Element.Attribute msg)
getCenterAttributes attributes =
    case List.Extra.find isCenter attributes of
        Just _ ->
            [ centerX, centerY ]

        Nothing ->
            []


isCenter : Attribute -> Bool
isCenter attribute =
    case attribute of
        Center ->
            True

        _ ->
            False


getAttribute : Attribute -> Maybe (Element.Attribute msg)
getAttribute attribute =
    case attribute of
        CenterX ->
            Just centerX

        CenterY ->
            Just centerY

        _ ->
            Nothing


getSize : List Attribute -> Int
getSize attributes =
    case List.Extra.find isSize attributes of
        Just (Size x) ->
            x

        _ ->
            10


isSize : Attribute -> Bool
isSize attribute =
    case attribute of
        Size _ ->
            True

        _ ->
            False


getColor : List Attribute -> Color
getColor attributes =
    case List.Extra.find isColor attributes of
        Just (Color x) ->
            x

        _ ->
            Color.black


isColor : Attribute -> Bool
isColor attribute =
    case attribute of
        Color _ ->
            True

        _ ->
            False
