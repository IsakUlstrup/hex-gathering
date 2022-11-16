module Entities.Timer exposing (Model, Msg(..), init, update, view)

import Html exposing (Html)
import Html.Events


type alias Model =
    Int


type Msg
    = Reset
    | Tick Int


init : Model
init =
    0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            0

        Tick _ ->
            model + 1


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Timer" ]
        , Html.p [] [ Html.text <| "Value: " ++ String.fromInt model ]
        , Html.button [ Html.Events.onClick Reset ] [ Html.text "Reset" ]
        ]
