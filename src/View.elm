module View exposing (..)

import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render
import Html
import Html.Attributes
import Html.Events
import HtmlExtra
import Player exposing (Player)
import Svg exposing (Attribute, Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Entity(..), Terrain(..), Tile(..))



-- CONSTANTS


animationDelayMultiplier : number
animationDelayMultiplier =
    150


svgClassList : List ( String, Bool ) -> Attribute msg
svgClassList classes =
    classes
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> List.intersperse " "
        |> String.concat
        |> Svg.Attributes.class



-- VIEW FUNCTIONS


viewTile : Point -> (Point -> msg) -> ( Point, Tile ) -> Svg msg
viewTile selectedPoint clickEvent ( point, tile ) =
    let
        wrapper cs =
            Svg.g
                [ svgClassList [ ( "hex", True ), ( "selected", point == selectedPoint ) ]
                , Svg.Events.onClick <| clickEvent point
                , Svg.Attributes.style
                    ("animation-delay: "
                        ++ (Point.distanceFloat ( 0, 0, 0 ) point
                                |> (*) animationDelayMultiplier
                                |> String.fromFloat
                           )
                        ++ "ms"
                    )
                ]
                cs
    in
    case tile of
        Terrain terrain ->
            wrapper
                [ viewTerrain ( point, terrain ) ]

        TerrainEntity terrain entity ->
            wrapper
                [ viewTerrain ( point, terrain )
                , viewEntity ( point, entity )
                ]


viewTerrain : ( Point, Terrain ) -> Svg msg
viewTerrain ( point, tile ) =
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
    in
    Svg.g
        [ Svg.Attributes.class "terrain"
        , Svg.Attributes.class tileType
        ]
        [ Svg.polygon
            [ Svg.Attributes.class "column-right"
            , Svg.Attributes.class "column-segment"
            , Svg.Attributes.points ([ points.p0, points.p1, Render.pointAdd points.p1 ( 0, columnHeight ), Render.pointAdd points.p0 ( 0, columnHeight ) ] |> Render.cornersToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "column-left"
            , Svg.Attributes.class "column-segment"
            , Svg.Attributes.points ([ points.p2, points.p3, Render.pointAdd points.p3 ( 0, columnHeight ), Render.pointAdd points.p2 ( 0, columnHeight ) ] |> Render.cornersToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "column-middle"
            , Svg.Attributes.class "column-segment"
            , Svg.Attributes.points ([ points.p1, points.p2, Render.pointAdd points.p2 ( 0, columnHeight ), Render.pointAdd points.p1 ( 0, columnHeight ) ] |> Render.cornersToString)
            ]
            []
        , Svg.polygon
            [ Svg.Attributes.class "face"
            , Svg.Attributes.points (points |> Render.cornersToString2)
            ]
            []
        ]


viewEntityHelper : List (Attribute msg) -> Char -> Svg msg
viewEntityHelper attrs icon =
    Svg.g
        [ Svg.Attributes.class "entity"
        ]
        [ Svg.text_
            ([ Svg.Attributes.class "content"
             , Svg.Attributes.x "2.5"
             , Svg.Attributes.y "2.5"
             ]
                ++ attrs
            )
            [ Svg.text (String.fromChar icon) ]
        ]


viewEntity : ( Point, Entity ) -> Svg msg
viewEntity ( _, entity ) =
    case entity of
        Resource icon ->
            viewEntityHelper [] icon

        NPC icon ->
            viewEntityHelper [] icon

        MapTransition _ ->
            viewEntityHelper [] '🚕'


positionNode : Point -> List (Attribute msg) -> List (Svg msg) -> Svg msg
positionNode position attributes children =
    let
        ( x, y ) =
            Render.pointToPixel position
    in
    Svg.g (Svg.Attributes.style ("transform: translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px);") :: attributes)
        children


viewPlayer : Player -> Svg msg
viewPlayer player =
    positionNode player.position
        [ Svg.Attributes.class (Player.moveStateString player), Svg.Attributes.class "entity" ]
        [ Svg.g [ Svg.Attributes.class "player-animation" ]
            [ Svg.text_ [ Svg.Attributes.class "content" ] [ Svg.text (player.icon |> String.fromChar) ]
            ]
        ]


viewHighlight : Point -> Svg msg
viewHighlight point =
    positionNode point
        [ Svg.Attributes.class "highlight" ]
        [ Svg.circle [ Svg.Attributes.class "move-target", Svg.Attributes.r "1", Svg.Attributes.fill "beige", Svg.Attributes.fillOpacity "0.9" ] [] ]


viewEntityInteractions : (String -> msg) -> ( Point, Entity ) -> Svg msg
viewEntityInteractions transitionEvent ( point, entity ) =
    let
        panelContents e =
            case e of
                Resource _ ->
                    [ Html.h1 [] [ Html.text "Resource" ]
                    , Html.p [] [ Html.text "Resource" ]
                    ]

                NPC _ ->
                    [ Html.h1 [] [ Html.text "Awesomesaur" ]
                    , Html.p [] [ Html.text "Awesomesaur" ]
                    ]

                MapTransition destination ->
                    [ Html.h1 [] [ Html.text "Taxi" ]
                    , Html.text ("Taxi to " ++ destination)
                    , Html.button [ Html.Events.onClick <| transitionEvent destination ] [ Html.text "Travel" ]
                    ]
    in
    HtmlExtra.dialog
        [ HtmlExtra.open
        , Html.Attributes.class "entity-interactions"
        ]
        (panelContents entity)


viewPlayerMoveTarget : Player -> Svg msg
viewPlayerMoveTarget player =
    Player.moveTarget player
        |> viewHighlight
