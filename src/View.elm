module View exposing (svgDefs, viewEntity, viewTile)

import AnimationConstants
import Character exposing (Character, CharacterMsg(..))
import HexEngine.Entity
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (HexCorners)
import Svg exposing (Attribute, Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Tile)


animationDelay : Point -> Attribute msg
animationDelay position =
    let
        distance : Float
        distance =
            Point.distanceFloat position ( 0, 0, 0 )
    in
    Svg.Attributes.style <| "animation-delay: " ++ String.fromFloat (distance * 150) ++ "ms"


svgClassList : List ( String, Bool ) -> Attribute msg
svgClassList classes =
    classes
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> List.intersperse " "
        |> String.concat
        |> Svg.Attributes.class



-- VIEW FUNCTIONS


viewTile : (Point -> msg) -> ( Point, Tile ) -> Svg msg
viewTile clickEvent ( point, tile ) =
    Svg.g
        [ svgClassList
            [ ( "hex", True )
            ]
        , Svg.Attributes.class <| Tile.tileToString tile
        ]
        (viewTerrain clickEvent ( point, tile ))


viewTerrainColumn : Float -> HexCorners -> Svg msg
viewTerrainColumn height corners =
    Svg.polygon
        [ Svg.Attributes.class "column"
        , [ corners.p0
          , corners.p1
          , corners.p2
          , corners.p3
          , Render.pointAdd corners.p3 ( 0, height )
          , Render.pointAdd corners.p2 ( 0, height )
          , Render.pointAdd corners.p1 ( 0, height )
          , Render.pointAdd corners.p0 ( 0, height )
          ]
            |> Render.cornerListToPoints
        ]
        []


viewTerrainEdges : Svg msg
viewTerrainEdges =
    Svg.path
        [ Svg.Attributes.class "edge"
        , Svg.Attributes.d
            -- "M -4.978 -0.009 C -4.986 -0.198 -4.659 -0.615 -4.636 -0.279 C -4.619 -0.033 -3.538 2.137 -2.889 3.342 C -2.826 3.459 -2.395 3.834 -2.319 3.858 C -2.245 3.881 -1.961 4.129 -1.508 4.157 C -1.328 4.168 2.017 4.172 2.225 4.062 C 2.507 3.913 4.611 0.477 4.721 0.03 C 4.755 -0.109 2.536 -4.002 2.371 -4.095 C 2.185 -4.199 -1.074 -4.075 -2.403 -4.15 C -2.464 -4.153 -3.943 -1.36 -4.491 -0.43 C -4.736 -0.014 -4.677 -0.429 -4.752 -0.393 C -4.916 -0.314 -2.606 -4.25 -2.514 -4.335 C -2.427 -4.415 2.511 -4.346 2.518 -4.335 C 2.61 -4.212 4.961 -0.151 4.986 0.009 C 5.201 -0.136 2.95 4.06 2.524 4.464 C 2.464 4.521 0.447 4.497 -0.043 4.478 C -1.596 4.418 -2.356 4.814 -2.581 4.778 C -2.806 4.742 -3.072 4.254 -3.123 3.929 C -3.17 3.625 -4.864 1.002 -4.978 -0.009 Z"
            "M 2.534 9.215 L 2.428 6.967 L 2.19 6.282 L 2.362 5.597 L 2.282 4.417 L -1.067 4.38 L -1.904 4.6 L -2.393 4.734 L -2.551 6.922 L -2.593 4.56 L -2.845 3.832 L -5.058 0 L -2.529 -4.38 L 2.529 -4.38 L 5.058 0 L 4.259 1.405 L 3.986 2.097 L 3.616 2.498 L 2.554 4.316 L 2.55 5.549 L 2.707 6.239 L 2.546 6.937 L 2.534 9.215 Z M 2.471 -4.28 L 0.154 -4.28 L -0.255 -4.176 L -0.618 -4.28 L -2.008 -4.28 L -2.467 -4.096 L -2.78 -3.745 L -4.942 0 L -2.969 3.339 L -2.313 3.971 L -1.544 4.28 L 1.969 4.28 L 2.382 4.17 L 2.67 3.935 L 3.533 2.441 L 3.704 1.788 L 4.17 1.337 L 4.942 0 L 3.668 -2.207 L 3.264 -2.729 L 3.089 -3.21 L 2.471 -4.28 Z"
        ]
        []


viewTerrainMask : HexCorners -> Svg msg
viewTerrainMask corners =
    Svg.polygon
        [ [ Render.pointAdd corners.p0 ( 0, 2 )
          , Render.pointAdd corners.p1 ( 0, 2 )
          , Render.pointAdd corners.p2 ( 0, 2 )
          , Render.pointAdd corners.p3 ( 0, 2 )
          , Render.pointAdd corners.p3 ( 0, 25 )
          , Render.pointAdd corners.p2 ( 0, 25 )
          , Render.pointAdd corners.p1 ( 0, 25 )
          , Render.pointAdd corners.p0 ( 0, 25 )
          ]
            |> Render.cornerListToPoints
        , Svg.Attributes.fill <| "url(#" ++ columnMaskId ++ ")"
        ]
        []


viewTerrain : (Point -> msg) -> ( Point, Tile ) -> List (Svg msg)
viewTerrain clickEvent ( position, _ ) =
    let
        corners : HexCorners
        corners =
            Render.generateHexCorners
    in
    [ Svg.g [ Svg.Attributes.class "animation", animationDelay position ]
        [ viewTerrainColumn 10 corners
        , Svg.polygon
            [ Svg.Attributes.class "face"
            , Render.cornersToPoints corners
            , Svg.Events.onClick <| clickEvent position
            ]
            []
        , viewTerrainEdges
        ]
    , viewTerrainMask corners
    ]


viewEntityActions : (CharacterMsg -> msg) -> Int -> CharacterMsg -> Svg msg
viewEntityActions actionMsg index action =
    let
        radius =
            6

        spread =
            60

        ( x, y ) =
            ( radius * sin (toFloat index * spread |> degrees)
            , radius * cos (toFloat index * spread |> degrees)
            )

        icon =
            case action of
                Travel _ ->
                    '🚌'
    in
    Svg.g
        [ Svg.Events.onClick <| actionMsg action
        , Svg.Attributes.class "action"
        , Svg.Attributes.style ("transition-delay: " ++ String.fromInt (AnimationConstants.playerMoveTime.value + (index * 200)) ++ "ms")
        ]
        [ Svg.circle
            [ Svg.Attributes.cx <| (x |> String.fromFloat)
            , Svg.Attributes.cy <| (y |> String.fromFloat)
            , Svg.Attributes.r "2.5"
            , Svg.Attributes.fill "beige"
            , Svg.Attributes.stroke "cyan"
            , Svg.Attributes.strokeWidth "0.3"
            ]
            []
        , Svg.text_
            [ Svg.Attributes.x <| (x |> String.fromFloat)
            , Svg.Attributes.y <| (y |> String.fromFloat)
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.fontSize "2pt"
            ]
            [ Svg.text <| String.fromChar icon ]
        ]


viewEntity : Maybe Point -> (CharacterMsg -> msg) -> (Point -> msg) -> ( Point, HexEngine.Entity.Entity Character ) -> Svg msg
viewEntity selectedPoint action clickEvent ( position, entity ) =
    let
        isSelected =
            case selectedPoint of
                Just p ->
                    p == position

                Nothing ->
                    False
    in
    Svg.g
        [ Svg.Attributes.class "enter-animation"
        , animationDelay position
        ]
        [ Svg.circle
            [ Svg.Attributes.x "0"
            , Svg.Attributes.y "0"
            , Svg.Attributes.r "1.5"
            , Svg.Attributes.class "shadow"
            ]
            []
        , Svg.g
            [ svgClassList
                [ ( "actions", True )
                , ( "active", isSelected )
                ]
            ]
            (List.indexedMap (viewEntityActions action) (entity.data.actions |> List.take 6))
        , Svg.g
            [ Svg.Attributes.class "animation"
            ]
            [ Svg.text_
                [ Svg.Attributes.class "content"
                , Svg.Events.onClick <| clickEvent position
                ]
                [ Svg.text <| String.fromChar entity.data.icon ]
            ]
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
