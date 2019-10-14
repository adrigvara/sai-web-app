module Grid exposing (elementsPerRow, emptyPadding, grid, padding, spacing)

import Element exposing (Element, column, el, fill, height, none, row, width)
import List.Extra


type Attribute
    = Padding Int
    | Spacing Int
    | RowSpacing Int
    | ColumnSpacing Int
    | ElementsPerRow Int
    | EmptyPadding Int


padding : Int -> Attribute
padding =
    Padding


spacing : Int -> Attribute
spacing =
    Spacing


elementsPerRow : Int -> Attribute
elementsPerRow =
    ElementsPerRow


emptyPadding : Int -> Attribute
emptyPadding =
    EmptyPadding


columnSpacing : Int -> Attribute
columnSpacing =
    ColumnSpacing


rowSpacing : Int -> Attribute
rowSpacing =
    RowSpacing


grid : List Attribute -> List (Element msg) -> Element msg
grid attributeList elementList =
    let
        n =
            getElementsPerRow attributeList
    in
    elementList
        |> fillSlots (getEmptyPadding attributeList) n
        |> List.Extra.groupsOf n
        |> List.map (row <| getRowAttributes <| attributeList)
        |> column (getColumnAttributes attributeList)


fillSlots : Int -> Int -> List (Element msg) -> List (Element msg)
fillSlots p n list =
    list
        ++ List.repeat
            (numberOfSlots n list)
            (empty p)


numberOfSlots : Int -> List (Element msg) -> Int
numberOfSlots n list =
    modBy n (List.length list)


empty : Int -> Element x
empty p =
    el [ Element.padding p, width fill, height fill ] none


getElementsPerRow : List Attribute -> Int
getElementsPerRow attributes =
    case List.Extra.find isElementsPerRow attributes of
        Just (ElementsPerRow n) ->
            n

        _ ->
            2


isElementsPerRow : Attribute -> Bool
isElementsPerRow attribute =
    case attribute of
        ElementsPerRow _ ->
            True

        _ ->
            False


getEmptyPadding : List Attribute -> Int
getEmptyPadding attributes =
    case List.Extra.find isEmptyPadding attributes of
        Just (EmptyPadding n) ->
            n

        _ ->
            0


isEmptyPadding : Attribute -> Bool
isEmptyPadding attribute =
    case attribute of
        EmptyPadding _ ->
            True

        _ ->
            False


getRowAttributes : List Attribute -> List (Element.Attribute msg)
getRowAttributes attributes =
    [ width fill ] ++ List.filterMap getRowAttribute attributes


getRowAttribute : Attribute -> Maybe (Element.Attribute msg)
getRowAttribute attribute =
    case attribute of
        Spacing x ->
            Just <| Element.spacing x

        ColumnSpacing x ->
            Just <| Element.spacing x

        _ ->
            Nothing


getColumnAttributes : List Attribute -> List (Element.Attribute msg)
getColumnAttributes attributes =
    [ width fill, height fill ]
        ++ List.filterMap getColumnAttribute attributes


getColumnAttribute : Attribute -> Maybe (Element.Attribute msg)
getColumnAttribute attribute =
    case attribute of
        Spacing x ->
            Just <| Element.spacing x

        RowSpacing x ->
            Just <| Element.spacing x

        Padding x ->
            Just <| Element.padding x

        _ ->
            Nothing
