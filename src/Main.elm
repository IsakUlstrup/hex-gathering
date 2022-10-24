module Main exposing (Model, Msg, main)

import Browser
import HexEngine.HexMap as HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events



-- MODEL


type Tile
    = Low
    | Medium
    | High


type Entity
    = Path


type alias Model =
    { map : HexMap Tile
    , entities : HexMap Entity
    , renderConfig : RenderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (HexMap.empty
            |> HexMap.insertReplaceHex ( ( 0, 0, 0 ), Medium )
            |> HexMap.insertReplaceHex ( ( 0, 1, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 1, 0, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 1, 1, -2 ), High )
            |> HexMap.insertReplaceHex ( ( -1, 1, 0 ), Low )
            |> HexMap.insertReplaceHex ( ( -1, 2, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( -1, 3, -2 ), Medium )
            |> HexMap.insertReplaceHex ( ( 0, 3, -3 ), Low )
            |> HexMap.insertReplaceHex ( ( 1, 2, -3 ), Low )
            |> HexMap.insertReplaceHex ( ( 1, 3, -4 ), Low )
            |> HexMap.insertReplaceHex ( ( 2, -1, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 3, -2, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 4, -2, -2 ), Medium )
            |> HexMap.insertReplaceHex ( ( 6, -3, -3 ), High )
            |> HexMap.insertReplaceHex ( ( 5, -2, -3 ), Medium )
            |> HexMap.insertReplaceHex ( ( -4, 2, 2 ), Medium )
            |> HexMap.insertReplaceHex ( ( -5, 3, 2 ), Medium )
            |> HexMap.insertReplaceHex ( ( -5, 2, 3 ), Medium )
            |> HexMap.insertReplaceHex ( ( -4, 3, 1 ), High )
            |> HexMap.insertReplaceHex ( ( 0, -1, 1 ), Low )
            |> HexMap.insertReplaceHex ( ( 1, -2, 1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 1, -1, 0 ), Medium )
            |> HexMap.insertReplaceHex ( ( 0, -2, 2 ), Medium )
            |> HexMap.insertReplaceHex ( ( 0, -3, 3 ), Medium )
        )
        HexMap.empty
        Render.initRenderConfig
    , Cmd.none
    )



-- UPDATE


type Msg
    = FocusTile Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusTile point ->
            ( { model | renderConfig = Render.withHexFocus point model.renderConfig }, Cmd.none )



-- VIEW


viewTile : ( Point, Tile ) -> Svg Msg
viewTile ( point, tile ) =
    let
        tileType =
            case tile of
                Low ->
                    "low"

                Medium ->
                    "medium"

                High ->
                    "high"

        points =
            Render.fancyHexCorners2

        columnHeight =
            case tile of
                Low ->
                    0.5

                Medium ->
                    2

                High ->
                    4

        animationDelayMultiplier =
            70
    in
    Svg.g
        [ Svg.Attributes.class "hex"
        , Svg.Attributes.class tileType
        , Svg.Attributes.style
            ("animation-delay: "
                ++ (HexEngine.Point.distanceFloat ( 0, 0, 0 ) point
                        |> (*) animationDelayMultiplier
                        |> String.fromFloat
                   )
                ++ "ms"
            )
        ]
        [ Svg.polygon
            [ Svg.Attributes.class "edge0"
            , Svg.Attributes.class "edge"
            , Svg.Attributes.points ([ points.p0, points.p1, Render.pointAdd points.p1 ( 0, columnHeight ), Render.pointAdd points.p0 ( 0, columnHeight ) ] |> Render.cornersToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "edge2"
            , Svg.Attributes.class "edge"
            , Svg.Attributes.points ([ points.p2, points.p3, Render.pointAdd points.p3 ( 0, columnHeight ), Render.pointAdd points.p2 ( 0, columnHeight ) ] |> Render.cornersToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "edge1"
            , Svg.Attributes.class "edge"
            , Svg.Attributes.points ([ points.p1, points.p2, Render.pointAdd points.p2 ( 0, columnHeight ), Render.pointAdd points.p1 ( 0, columnHeight ) ] |> Render.cornersToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "face"
            , Svg.Attributes.points (points |> Render.cornersToString2)
            , Svg.Events.onClick <| FocusTile point
            ]
            []
        ]


view : Model -> Html Msg
view model =
    main_ []
        [ Render.renderGrid [] model.renderConfig model.map viewTile
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
