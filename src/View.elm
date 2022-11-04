module View exposing (..)

import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render
import Html
import Html.Attributes
import Html.Events
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


viewEntityInteractions : (String -> msg) -> Maybe ( Point, Entity ) -> Svg msg
viewEntityInteractions transitionEvent mbyEntity =
    let
        ( width, height ) =
            ( 40, 30 )

        panelContents e =
            case e of
                Resource _ ->
                    [ Html.text "Resource" ]

                NPC _ ->
                    [ Html.text "Vilde the Flottosaur" ]

                MapTransition destination ->
                    [ Html.text ("Taxi to " ++ destination)
                    , Html.button [ Html.Events.onClick <| transitionEvent destination ] [ Html.text "Travel" ]
                    ]
    in
    case mbyEntity of
        Just ( point, entity ) ->
            positionNode point
                []
                [ Svg.foreignObject
                    [ Svg.Attributes.width <| String.fromInt width
                    , Svg.Attributes.height <| String.fromInt height
                    , Svg.Attributes.x <| String.fromInt -(width // 2)
                    , Svg.Attributes.y <| String.fromInt -(height + height // 3)
                    , Svg.Attributes.pointerEvents "none"
                    ]
                    [ Html.aside [ Html.Attributes.class "interaction-container", Html.Attributes.attribute "xmlns" "http://www.w3.org/1999/xhtml" ]
                        [ Html.div [ Html.Attributes.class "entity-interactions" ] (panelContents entity)
                        ]
                    ]
                ]

        Nothing ->
            positionNode ( 0, 0, 0 )
                []
                []


viewPlayerMoveTarget : Player -> Svg msg
viewPlayerMoveTarget player =
    Player.moveTarget player
        |> viewHighlight
