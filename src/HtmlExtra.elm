module HtmlExtra exposing (dialog, open)

import Html exposing (Attribute, Html)
import Html.Attributes


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attributes children =
    Html.node "dialog" attributes children


open : Attribute msg
open =
    Html.Attributes.attribute "open" ""
