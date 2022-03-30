module Helpers exposing (..)

import Html
import Html.Attributes exposing (classList)


classes : List String -> Html.Attribute msg
classes =
    classList << List.map (\c -> ( c, True ))


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


fork : (a -> b) -> (a -> c) -> a -> ( b, c )
fork fab fac a =
    ( fab a, fac a )
