module Entities.Counter exposing (Model, Msg(..), init, update, view)

import Html exposing (Html)


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
update _ model =
    model


view : Model -> Html Msg
view _ =
    Html.div [] [ Html.text "Counter" ]
