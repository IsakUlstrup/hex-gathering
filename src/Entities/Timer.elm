module Entities.Timer exposing (..)

import Html exposing (Html)


type alias Model =
    Int


type Msg
    = Reset
    | Tick Int


init : Model
init =
    0


update : Msg -> Model -> Model
update _ model =
    model


view : Model -> Html Msg
view _ =
    Html.div [] [ Html.text "Timer" ]
