module View exposing (viewEntity, viewTile)

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



-- CONSTANTS


animationDelayMultiplier : number
animationDelayMultiplier =
    150


animationDelay : Point -> Attribute msg
animationDelay position =
    let
        delay d =
            if d == 0 then
                "0"

            else
                d
                    |> (*) animationDelayMultiplier
                    |> (+) (toFloat <| AnimationConstants.playerMoveTime.value)
                    |> String.fromFloat
    in
    Svg.Attributes.style
        ("animation-delay: "
            ++ (delay <| Point.distanceFloat ( 0, 0, 0 ) position)
            ++ "ms"
        )


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
        wrapper : List (Html msg) -> Html msg
        wrapper cs =
            Svg.g
                [ svgClassList
                    [ ( "hex", True )
                    , ( "selected", meq selectedPoint point )
                    , ( "player", point == playerPosition )
                    ]
                , animationDelay point
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
        (viewTerrain clickEvent ( point, tile ) :: marker)



-- case tile of
--     Terrain terrain ->
--         wrapper
--             (viewTerrain ( point, terrain ) :: marker)
--     TerrainEntity terrain entity ->
--         wrapper
--             (viewTerrain ( point, terrain ) :: marker ++ [ viewEntity ( point, entity ) ])


viewMarker : Svg msg
viewMarker =
    Svg.g [ Svg.Attributes.class "marker" ]
        [ Svg.circle
            [ Svg.Attributes.r "1"
            , Svg.Attributes.cx "2.5"
            , Svg.Attributes.cy "2.5"
            , Svg.Attributes.class "marker-dot"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.r "1"
            , Svg.Attributes.cx "2.5"
            , Svg.Attributes.cy "2.5"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.class "marker-circle"
            ]
            []
        ]


viewTerrain : (Point -> msg) -> ( Point, Tile ) -> Svg msg
viewTerrain clickEvent ( position, tile ) =
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

        points : HexCorners
        points =
            Render.hardcodedPoints

        columnHeight : Float
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



-- viewEntityHelper : List (Attribute msg) -> Char -> Svg msg
-- viewEntityHelper attrs icon =
--     Svg.g
--         [ Svg.Attributes.class "entity"
--         ]
--         [ Svg.text_
--             ([ Svg.Attributes.class "content"
--              , Svg.Attributes.x "2.5"
--              , Svg.Attributes.y "2.5"
--              ]
--                 ++ attrs
--             )
--             [ Svg.text (String.fromChar icon) ]
--         ]
-- viewEntity : ( Point, Entity ) -> Svg msg
-- viewEntity ( _, entity ) =
--     -- case entity of
--     --     Counter _ ->
--     --         viewEntityHelper [] '🧮'
--     --     Timer _ ->
--     --         viewEntityHelper [] '⏲'
--     --     Entity.Player _ ->
--     --         viewEntityHelper [] '🐼'
--     viewEntityHelper [] entity
-- positionNode : Point -> List (Attribute msg) -> List (Svg msg) -> Svg msg
-- positionNode position attributes children =
--     let
--         ( x, y ) =
--             Render.pointToPixel position
--     in
--     Svg.g (Svg.Attributes.style ("transform: translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px);") :: attributes)
--         children
-- viewPlayer : Player -> Svg msg
-- viewPlayer player =
--     positionNode player.position
--         [ Svg.Attributes.class (Player.stateString player), Svg.Attributes.class "entity" ]
--         [ Svg.g [ Svg.Attributes.class "player-animation" ]
--             [ Svg.text_ [ Svg.Attributes.class "content" ] [ Svg.text (player.icon |> String.fromChar) ]
--             ]
--         ]


viewEntity : (Point -> msg) -> ( Point, HexEngine.Entity.Entity Entity ) -> Svg msg
viewEntity clickEvent ( position, entity ) =
    Svg.g
        [ Svg.Attributes.class (HexEngine.Entity.stateString entity)
        , Svg.Events.onClick <| clickEvent position
        ]
        [ Svg.g
            [ Svg.Attributes.class "animation"
            , animationDelay position
            ]
            [ Svg.text_ [ Svg.Attributes.class "content", Svg.Attributes.x "2.5", Svg.Attributes.y "2.5" ] [ Svg.text (entity.data |> String.fromChar) ] ]
        ]
