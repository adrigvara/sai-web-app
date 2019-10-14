module Device exposing (fromFlags, get, sub)

import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Element exposing (Device, DeviceClass(..), Orientation(..), classifyDevice)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import Task exposing (perform)


type alias WindowSize =
    { width : Int
    , height : Int
    }


get : (Device -> msg) -> Cmd msg
get msgFromDevice =
    perform (msgFromViewport msgFromDevice) getViewport


msgFromViewport : (Device -> msg) -> (Viewport -> msg)
msgFromViewport msgFromDevice =
    msgFromDevice << classifyDevice << windowSizeFromViewport


windowSizeFromViewport : Viewport -> WindowSize
windowSizeFromViewport { viewport } =
    WindowSize (round viewport.width) (round viewport.height)


sub : (Device -> msg) -> Sub msg
sub deviceToMsg =
    onResize <| compose WindowSize classifyDevice deviceToMsg


compose : (a -> b -> c) -> (c -> d) -> (d -> e) -> a -> b -> e
compose f1 f2 f3 x y =
    f3 <| f2 <| f1 x y


fromFlags : Value -> Device
fromFlags flags =
    case decodeValue windowSizeDecoder flags of
        Ok windowSize ->
            classifyDevice windowSize

        Err _ ->
            default


windowSizeDecoder : Decoder WindowSize
windowSizeDecoder =
    Decode.map2 WindowSize
        (Decode.field "width" Decode.int)
        (Decode.field "heigth" Decode.int)


default : Device
default =
    Device Desktop Landscape
