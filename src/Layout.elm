module Layout exposing (grid)

import Element exposing (..)
import List.Extra


grid :
    List (Attribute x)
    -> List (Attribute x)
    -> Int
    -> (a -> Element x)
    -> Int
    -> List a
    -> Element x
grid columnRules rowRules emptyPadding toElement n list =
    list
        |> List.map toElement
        |> fillSlots emptyPadding n
        |> List.Extra.groupsOf n
        |> List.map (row rowRules)
        |> column columnRules


fillSlots : Int -> Int -> List (Element x) -> List (Element x)
fillSlots emptyPadding n list =
    list
        ++ List.repeat
            (numberOfSlots n list)
            (empty emptyPadding)


numberOfSlots : Int -> List (Element x) -> Int
numberOfSlots n list =
    modBy n (List.length list)


empty : Int -> Element x
empty emptyPadding =
    el [ padding emptyPadding, width fill, height fill ] none
