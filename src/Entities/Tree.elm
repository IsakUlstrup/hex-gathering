module Entities.Tree exposing (Model, Msg, init, update, view)

import Html exposing (Html)



-- MODEL


init : String -> Model
init name =
    Model '🌲' name True


type alias Model =
    { icon : Char
    , name : String
    , alive : Bool
    }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text model.name ]
        , Html.p [] [ Html.text "A super nice tree" ]
        , Html.p []
            [ Html.text <|
                if model.alive then
                    "Alive"

                else
                    "Dead"
            ]
        , Html.button [] [ Html.text "Harvest" ]
        ]



-- UPDATE


type Msg
    = Harvest


update : Msg -> Model -> Model
update msg model =
    case msg of
        Harvest ->
            { model | alive = False }
