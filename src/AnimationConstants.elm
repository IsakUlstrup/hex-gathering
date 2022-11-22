module AnimationConstants exposing (Constant, delayMultiplier, fallDuration, playerMoveTime, styleNode)

import Html exposing (Html)


type alias Constant =
    { label : String
    , value : Int
    }


playerMoveTime : Constant
playerMoveTime =
    Constant "player-move-duration" 500


fallDuration : Constant
fallDuration =
    Constant "fall-duration" 500


delayMultiplier : Constant
delayMultiplier =
    Constant "delay-multiplier" 100


toCssVar : Constant -> String
toCssVar constant =
    "--" ++ constant.label ++ ": " ++ String.fromInt constant.value ++ "ms;"


styleNode : List Constant -> Html msg
styleNode constants =
    Html.node "style"
        []
        [ Html.text <| ":root { " ++ (constants |> List.map toCssVar |> List.intersperse " " |> String.concat) ++ " }" ]
