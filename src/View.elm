module View exposing (viewEntity, viewTile)

-- import AnimationConstants

import AnimationConstants
import Entity exposing (Entity)
import HexEngine.Entity
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (HexCorners)
import Html exposing (Html)
import Svg exposing (Attribute, Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Tile(..))


animationDelay : Point -> Attribute msg
animationDelay position =
    let
        distance =
            Point.distanceFloat position ( 0, 0, 0 )
    in
    Svg.Attributes.style <| "animation-delay: " ++ String.fromFloat (distance * 100 + toFloat AnimationConstants.playerMoveTime.value) ++ "ms"


svgClassList : List ( String, Bool ) -> Attribute msg
svgClassList classes =
    classes
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> List.intersperse " "
        |> String.concat
        |> Svg.Attributes.class



-- VIEW FUNCTIONS


meq : Maybe a -> a -> Bool
meq m a =
    case m of
        Just x ->
            x == a

        Nothing ->
            False


viewTile : Point -> Maybe Point -> (Point -> msg) -> ( Point, Tile ) -> Svg msg
viewTile playerPosition selectedPoint clickEvent ( point, tile ) =
    let
        tileType : String
        tileType =
            case tile of
                Low ->
                    "low"

                Medium ->
                    "medium"

                High ->
                    "high"

        wrapper : List (Html msg) -> Html msg
        wrapper cs =
            Svg.g
                [ svgClassList
                    [ ( "hex", True )
                    , ( "selected", meq selectedPoint point )
                    , ( "player", point == playerPosition )
                    ]
                , Svg.Attributes.class tileType
                ]
                cs

        marker : List (Svg msg)
        marker =
            if meq selectedPoint point then
                [ viewMarker ]

            else
                []
    in
    wrapper
        (viewTerrain clickEvent ( point, tile ) ++ marker)


viewMarker : Svg msg
viewMarker =
    Svg.g [ Svg.Attributes.class "marker" ]
        [ Svg.circle
            [ Svg.Attributes.r "1"
            , Svg.Attributes.class "marker-dot"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.r "1"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.class "marker-circle"
            ]
            []
        ]


viewTerrain : (Point -> msg) -> ( Point, Tile ) -> List (Svg msg)
viewTerrain clickEvent ( position, tile ) =
    let
        points : HexCorners
        points =
            Render.generateHexCorners

        columnHeight : Float
        columnHeight =
            case tile of
                Low ->
                    8.5

                Medium ->
                    10

                High ->
                    12
    in
    [ Svg.defs []
        [ Svg.radialGradient
            [ Svg.Attributes.id "column-mask"
            , Svg.Attributes.gradientTransform "translate(-0.5, -1.7) scale(2)"
            ]
            [ Svg.stop
                [ Svg.Attributes.stopColor "red"
                , Svg.Attributes.stopColor "rgb(254, 255, 221)"
                , Svg.Attributes.stopOpacity "0"
                , Svg.Attributes.offset "90%"
                ]
                []
            , Svg.stop
                [ Svg.Attributes.stopColor "blue"
                , Svg.Attributes.stopColor "rgb(254, 255, 221)"
                , Svg.Attributes.offset "100%"
                ]
                []
            ]
        ]
    , Svg.g [ Svg.Attributes.class "animation", animationDelay position ]
        [ Svg.polygon
            [ Svg.Attributes.class "column-right"
            , Svg.Attributes.class "column-segment"
            , Svg.Attributes.points ([ points.p0, points.p1, Render.pointAdd points.p1 ( 0, columnHeight ), Render.pointAdd points.p0 ( 0, columnHeight ) ] |> Render.cornerListToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "column-left"
            , Svg.Attributes.class "column-segment"
            , Svg.Attributes.points ([ points.p2, points.p3, Render.pointAdd points.p3 ( 0, columnHeight ), Render.pointAdd points.p2 ( 0, columnHeight ) ] |> Render.cornerListToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "column-middle"
            , Svg.Attributes.class "column-segment"
            , Svg.Attributes.points ([ points.p1, points.p2, Render.pointAdd points.p2 ( 0, columnHeight ), Render.pointAdd points.p1 ( 0, columnHeight ) ] |> Render.cornerListToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "face"
            , Svg.Attributes.points (points |> Render.cornersToString)
            , Svg.Events.onClick <| clickEvent position
            ]
            []
        ]
    , Svg.polygon
        [ Svg.Attributes.points
            ([ Render.pointAdd points.p0 ( 0, 2 )
             , Render.pointAdd points.p1 ( 0, 2 )
             , Render.pointAdd points.p2 ( 0, 2 )
             , Render.pointAdd points.p3 ( 0, 2 )
             , Render.pointAdd points.p3 ( 0, 25 )
             , Render.pointAdd points.p2 ( 0, 25 )
             , Render.pointAdd points.p1 ( 0, 25 )
             , Render.pointAdd points.p0 ( 0, 25 )
             ]
                |> Render.cornerListToString
            )
        , Svg.Attributes.fill "url(#column-mask)"
        ]
        []
    ]


viewEntity : (Point -> msg) -> ( Point, HexEngine.Entity.Entity Entity ) -> Svg msg
viewEntity clickEvent ( position, entity ) =
    Svg.g
        [ Svg.Attributes.class "animation"
        ]
        [ Svg.text_
            [ Svg.Attributes.class "content"
            , animationDelay position
            , Svg.Events.onClick <| clickEvent position
            ]
            [ Svg.text (entity.data |> String.fromChar) ]
        ]
