module Device exposing (default, fromFlags, get, sub)

import Element exposing (Device, classifyDevice)
import Json.Decode as Decode exposing (Value)
import Window
import Window.Size


get : (Device -> msg) -> Cmd msg
get toMsg =
    Window.Size.get <| toMsg << classifyDevice


sub : (Device -> msg) -> Sub msg
sub toMsg =
    Window.Size.sub <| toMsg << classifyDevice


default : Device
default =
    classifyDevice Window.Size.default


fromFlags : Value -> Device
fromFlags =
    classifyDevice << Window.Size.fromFlags
