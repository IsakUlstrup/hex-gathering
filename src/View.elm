module View exposing (svgDefs, viewEntity, viewTile)

-- import AnimationConstants

import AnimationConstants
import Entity exposing (Entity)
import HexEngine.Entity
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (HexCorners)
import Svg exposing (Attribute, Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Tile(..))


animationDelay : Point -> Attribute msg
animationDelay position =
    let
        distance : Float
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
        marker : List (Svg msg)
        marker =
            if meq selectedPoint point then
                [ viewMarker ]

            else
                []
    in
    Svg.g
        [ svgClassList
            [ ( "hex", True )
            , ( "selected", meq selectedPoint point )
            , ( "player", point == playerPosition )
            ]
        , Svg.Attributes.class <| Tile.tileToString tile
        ]
        (viewTerrain clickEvent ( point, tile ) ++ marker)


viewTerrainColumn : Float -> HexCorners -> Svg msg
viewTerrainColumn height corners =
    Svg.polygon
        [ Svg.Attributes.class "column"
        , Svg.Attributes.points
            ([ corners.p0
             , corners.p1
             , corners.p2
             , corners.p3
             , Render.pointAdd corners.p3 ( 0, height )
             , Render.pointAdd corners.p2 ( 0, height )
             , Render.pointAdd corners.p1 ( 0, height )
             ]
                |> Render.cornerListToString
            )
        ]
        []


viewTerrainMask : HexCorners -> Svg msg
viewTerrainMask corners =
    Svg.polygon
        [ Svg.Attributes.points
            ([ Render.pointAdd corners.p0 ( 0, 2 )
             , Render.pointAdd corners.p1 ( 0, 2 )
             , Render.pointAdd corners.p2 ( 0, 2 )
             , Render.pointAdd corners.p3 ( 0, 2 )
             , Render.pointAdd corners.p3 ( 0, 25 )
             , Render.pointAdd corners.p2 ( 0, 25 )
             , Render.pointAdd corners.p1 ( 0, 25 )
             , Render.pointAdd corners.p0 ( 0, 25 )
             ]
                |> Render.cornerListToString
            )
        , Svg.Attributes.fill <| "url(#" ++ columnMaskId ++ ")"
        ]
        []


viewTerrain : (Point -> msg) -> ( Point, Tile ) -> List (Svg msg)
viewTerrain clickEvent ( position, tile ) =
    let
        corners : HexCorners
        corners =
            Render.generateHexCorners
    in
    [ Svg.g [ Svg.Attributes.class "animation", animationDelay position ]
        [ viewTerrainColumn 10 corners
        , Svg.polygon
            [ Svg.Attributes.class "face"
            , Svg.Attributes.points (corners |> Render.cornersToString)
            , Svg.Events.onClick <| clickEvent position
            ]
            []
        ]
    , viewTerrainMask corners
    ]


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


columnMaskId : String
columnMaskId =
    "column-mask"


svgDefs : Svg msg
svgDefs =
    Svg.defs []
        [ Svg.radialGradient
            [ Svg.Attributes.id columnMaskId
            , Svg.Attributes.gradientTransform "translate(-0.5, -1.7) scale(2)"
            ]
            [ Svg.stop
                [ Svg.Attributes.stopColor "rgb(254, 255, 221)"
                , Svg.Attributes.stopOpacity "0"
                , Svg.Attributes.offset "90%"
                ]
                []
            , Svg.stop
                [ Svg.Attributes.stopColor "rgb(254, 255, 221)"
                , Svg.Attributes.offset "100%"
                ]
                []
            ]
        ]
