module View exposing (..)

import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render
import Player exposing (Player)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Entity(..), Tile(..))



-- CONSTANTS


animationDelayMultiplier : number
animationDelayMultiplier =
    150



-- VIEW FUNCTIONS


viewTile : (Point -> msg) -> ( Point, Tile ) -> Svg msg
viewTile clickEvent ( point, tile ) =
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
            Render.hardcodedPoints

        columnHeight =
            case tile of
                Low ->
                    0.5

                Medium ->
                    2

                High ->
                    4

        events =
            case tile of
                Medium ->
                    [ Svg.Events.onClick (clickEvent point)
                    ]

                _ ->
                    []
    in
    Svg.g
        ([ Svg.Attributes.class "hex"
         , Svg.Attributes.class tileType
         , Svg.Attributes.style
            ("animation-delay: "
                ++ (Point.distanceFloat ( 0, 0, 0 ) point
                        |> (*) animationDelayMultiplier
                        |> String.fromFloat
                   )
                ++ "ms"
            )
         ]
            ++ events
        )
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
            ]
            []
        ]


viewEntity : ( Point, Entity ) -> Svg msg
viewEntity ( point, entity ) =
    case entity of
        Resource icon ->
            Svg.g
                [ Svg.Attributes.class "resource-animation"
                , Svg.Attributes.style
                    ("animation-delay: "
                        ++ (Point.distanceFloat ( 0, 0, 0 ) point
                                |> (*) animationDelayMultiplier
                                |> String.fromFloat
                           )
                        ++ "ms"
                    )
                ]
                [ Svg.text_
                    [ Svg.Attributes.class "resource"
                    , Svg.Attributes.x "2.5"
                    , Svg.Attributes.y "2.5"
                    ]
                    [ Svg.text (String.fromChar icon) ]
                ]

        NPC icon ->
            Svg.g
                [ Svg.Attributes.class "resource-animation"
                , Svg.Attributes.style
                    ("animation-delay: "
                        ++ (Point.distanceFloat ( 0, 0, 0 ) point
                                |> (*) animationDelayMultiplier
                                |> String.fromFloat
                           )
                        ++ "ms"
                    )
                ]
                [ Svg.text_
                    [ Svg.Attributes.class "resource"
                    , Svg.Attributes.x "2.5"
                    , Svg.Attributes.y "2.5"
                    ]
                    [ Svg.text (String.fromChar icon) ]
                ]


viewPlayer : Player -> Svg msg
viewPlayer player =
    let
        ( x, y ) =
            Render.pointToPixel player.position
    in
    Svg.g [ Svg.Attributes.class (Player.moveStateString player), Svg.Attributes.class "player", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px);") ]
        [ Svg.g [ Svg.Attributes.class "player-animation" ] [ Svg.text_ [ Svg.Attributes.class "player-icon" ] [ Svg.text (player.icon |> String.fromChar) ] ] ]


viewHighlight : Point -> Svg msg
viewHighlight point =
    let
        ( x, y ) =
            Render.pointToPixel point
    in
    Svg.g [ Svg.Attributes.class "highlight", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px);") ]
        [ Svg.circle [ Svg.Attributes.class "move-target", Svg.Attributes.r "1", Svg.Attributes.fill "beige", Svg.Attributes.fillOpacity "0.9" ] [] ]


viewPlayerMoveTarget : Player -> Svg msg
viewPlayerMoveTarget player =
    Player.moveTarget player
        |> viewHighlight
