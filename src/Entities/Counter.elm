module Entities.Counter exposing (Model, Msg(..), init, update, view)

import Html exposing (Html)
import Html.Events


type alias Model =
    Int


type Msg
    = Increment
    | Decrement
    | Reset


init : Model
init =
    0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1

        Reset ->
            0


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Counter" ]
        , Html.p [] [ Html.text <| "Value: " ++ String.fromInt model ]
        , Html.button [ Html.Events.onClick Increment ] [ Html.text "Increment" ]
        , Html.button [ Html.Events.onClick Decrement ] [ Html.text "Decrement" ]
        , Html.button [ Html.Events.onClick Reset ] [ Html.text "Reset" ]
        ]
