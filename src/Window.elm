module Window exposing (Size, onResize, size)

import Browser.Events
import Json.Decode as Decode exposing (Decoder, Value, field, int)


type alias Size =
    { width : Int
    , height : Int
    }


onResize : (Int -> Int -> msg) -> Sub msg
onResize =
    Browser.Events.onResize


size : Value -> Size
size flags =
    case Decode.decodeValue sizeDecoder flags of
        Ok windowSize ->
            windowSize

        Err _ ->
            Size 0 0


sizeDecoder : Decoder Size
sizeDecoder =
    Decode.map2 Size
        (field "width" int)
        (field "height" int)
