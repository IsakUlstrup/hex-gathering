module AnimationConstants exposing (fallDuration, playerMoveTime, styleNode)

import Html exposing (Html)


playerMoveTime : ( String, Int )
playerMoveTime =
    ( "player-move-duration", 500 )


fallDuration : ( String, Int )
fallDuration =
    ( "fall-duration", 1200 )


toString : ( String, Int ) -> String
toString ( varName, duration ) =
    "--" ++ varName ++ ": " ++ String.fromInt duration ++ "ms;"


styleNode : List ( String, Int ) -> Html msg
styleNode variables =
    Html.node "style"
        []
        [ Html.text <| ":root { " ++ (variables |> List.map toString |> List.intersperse " " |> String.concat) ++ " }" ]