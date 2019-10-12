module Window.Size exposing (default, fromFlags, get, sub)

import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import Task exposing (perform)
import Window


get : (Window.Size -> msg) -> Cmd msg
get toMsg =
    getWith <| toMsg << fromViewport


getWith : (Viewport -> msg) -> Cmd msg
getWith toMsg =
    perform toMsg getViewport


fromViewport : Viewport -> Window.Size
fromViewport { viewport } =
    Window.Size (round viewport.width) (round viewport.height)


sub : (Window.Size -> msg) -> Sub msg
sub toMsg =
    onResize (composeSub Window.Size toMsg)


composeSub : (a -> b -> c) -> (c -> d) -> a -> b -> d
composeSub f1 f2 x y =
    f2 <| f1 x y


default : Window.Size
default =
    Window.Size 1280 768


fromFlags : Value -> Window.Size
fromFlags flags =
    case decodeValue decoder flags of
        Ok windowSize ->
            windowSize

        Err _ ->
            default


decoder : Decoder Window.Size
decoder =
    Decode.map2 Window.Size
        (Decode.field "width" Decode.int)
        (Decode.field "heigth" Decode.int)
