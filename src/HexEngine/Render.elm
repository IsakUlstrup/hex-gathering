module HexEngine.Render exposing
    ( HexCorners
    , RenderConfig
    , cornerListToString
    , cornersToString
    , entityMap
    , entityMap2
    , hardcodedPoints
    , initRenderConfig
    , pointAdd
    , pointToPixel
    , renderMap
    , withHexFocus
    , withZoom
    )

import HexEngine.EntityMap as EntityMap exposing (EntityMap)
import HexEngine.HexGrid as HexGrid exposing (HexGrid)
import HexEngine.Point as Point exposing (Point)
import Svg exposing (Svg, g, svg)
import Svg.Attributes
import Svg.Keyed
import Svg.Lazy



---- CONFIG BUILDER ----


type alias RenderConfig =
    { cameraX : Float
    , cameraY : Float
    , zoom : Float
    }


initRenderConfig : RenderConfig
initRenderConfig =
    RenderConfig 0 0 1


withCameraPosition : ( Float, Float ) -> RenderConfig -> RenderConfig
withCameraPosition ( x, y ) config =
    { config | cameraX = x, cameraY = y }


{-| move camera to focus on point
-}
withHexFocus : Point -> RenderConfig -> RenderConfig
withHexFocus point config =
    let
        pos : ( Float, Float )
        pos =
            point |> pointToPixel
    in
    config |> withCameraPosition pos


withZoom : Float -> RenderConfig -> RenderConfig
withZoom zoom config =
    { config | zoom = zoom }



-- HELPERS


{-| Hex size constant
-}
hexSize : Float
hexSize =
    5


{-| Get the center of a given point in screen coordinates
-}
pointToPixel : Point -> ( Float, Float )
pointToPixel point =
    let
        ( q, r ) =
            Point.toAxial point
    in
    ( hexSize * (3 / 2 * toFloat q)
    , hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r)
    )


pointToYpos : Point -> Float
pointToYpos point =
    pointToPixel point |> Tuple.second


cornerListToString : List ( Float, Float ) -> String
cornerListToString points =
    let
        tupleToString : ( Float, Float ) -> String
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    List.map tupleToString points |> List.intersperse " " |> String.concat


cornersToString : HexCorners -> String
cornersToString points =
    let
        tupleToString : ( Float, Float ) -> String
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    [ tupleToString points.p0
    , tupleToString points.p1
    , tupleToString points.p2
    , tupleToString points.p3
    , tupleToString points.p4
    , tupleToString points.p5
    ]
        |> List.intersperse " "
        |> String.concat


type alias HexCorners =
    { p0 : ( Float, Float )
    , p1 : ( Float, Float )
    , p2 : ( Float, Float )
    , p3 : ( Float, Float )
    , p4 : ( Float, Float )
    , p5 : ( Float, Float )
    }


pointAdd : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
pointAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


hardcodedPoints : HexCorners
hardcodedPoints =
    HexCorners
        ( 7.5, 2.5 )
        ( 5, 6.830127018922193 )
        ( 8.881784197001252e-16, 6.8301270189221945 )
        ( -2.5, 2.5000000000000004 )
        ( -2.220446049250313e-15, -1.8301270189221919 )
        ( 5, -1.8301270189221928 )



-- RENDER


renderHex : (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> Svg msg
renderHex renderTile ( point, t ) =
    let
        ( x, y ) =
            pointToPixel point
    in
    g [ Svg.Attributes.class "point", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat (x - hexSize / 2) ++ "px, " ++ String.fromFloat (y - hexSize / 2) ++ "px);") ]
        [ renderTile ( point, t ) ]


keyedViewHex : (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> ( String, Svg msg )
keyedViewHex renderTile tile =
    ( Point.toString (Tuple.first tile)
    , Svg.Lazy.lazy (renderHex renderTile) tile
    )


renderLayer : List ( Point, tile ) -> (( Point, tile ) -> Svg msg) -> Svg msg
renderLayer tiles renderTile =
    Svg.Keyed.node "g"
        [ Svg.Attributes.class "layer" ]
        -- sort by y position and render
        (tiles
            |> List.sortBy (Tuple.first >> pointToYpos)
            |> List.map (keyedViewHex renderTile)
        )


renderMap : RenderConfig -> HexGrid tile -> (( Point, tile ) -> Svg msg) -> List (Svg msg) -> Svg msg
renderMap config map renderTile extras =
    svg
        [ Svg.Attributes.viewBox ([ -50, -50, 100, 100 ] |> List.map String.fromFloat |> List.intersperse " " |> String.concat)
        , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
        ]
        [ Svg.g
            [ Svg.Attributes.style
                ("transform: translate("
                    ++ String.fromFloat -(config.cameraX * config.zoom)
                    ++ "px, "
                    ++ String.fromFloat -(config.cameraY * config.zoom)
                    ++ "px) scale("
                    ++ String.fromFloat config.zoom
                    ++ ");"
                )
            , Svg.Attributes.class "camera"
            ]
            [ Svg.Lazy.lazy2 renderLayer
                (map |> HexGrid.toList)
                renderTile
            , Svg.g [] extras
            ]
        ]


entityMap :
    RenderConfig
    -> HexGrid tile
    -> (( Point, tile ) -> Svg msg)
    -> List ( Point, e )
    -> (( Point, e ) -> Svg msg)
    -> Svg msg
entityMap config grid renderTile entities renderEntity =
    svg
        [ Svg.Attributes.viewBox ([ -50, -50, 100, 100 ] |> List.map String.fromFloat |> List.intersperse " " |> String.concat)
        , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
        ]
        [ Svg.g
            [ Svg.Attributes.style
                ("transform: translate("
                    ++ String.fromFloat -(config.cameraX * config.zoom)
                    ++ "px, "
                    ++ String.fromFloat -(config.cameraY * config.zoom)
                    ++ "px) scale("
                    ++ String.fromFloat config.zoom
                    ++ ");"
                )
            , Svg.Attributes.class "camera"
            ]
            [ Svg.Lazy.lazy2 renderLayer
                (grid |> HexGrid.toList)
                renderTile
            , Svg.Lazy.lazy2 renderLayer
                entities
                renderEntity
            ]
        ]


entityMap2 :
    RenderConfig
    -> EntityMap tile entity
    -> (( Point, tile ) -> Svg msg)
    -> (( Point, entity ) -> Svg msg)
    -> Svg msg
entityMap2 config map renderTile renderEntity =
    svg
        [ Svg.Attributes.viewBox ([ -50, -50, 100, 100 ] |> List.map String.fromFloat |> List.intersperse " " |> String.concat)
        , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
        ]
        [ Svg.g
            [ Svg.Attributes.style
                ("transform: translate("
                    ++ String.fromFloat -(config.cameraX * config.zoom)
                    ++ "px, "
                    ++ String.fromFloat -(config.cameraY * config.zoom)
                    ++ "px) scale("
                    ++ String.fromFloat config.zoom
                    ++ ");"
                )
            , Svg.Attributes.class "camera"
            ]
            [ Svg.Lazy.lazy2 renderLayer
                (EntityMap.gridList map)
                renderTile
            , Svg.Lazy.lazy2 renderLayer
                (EntityMap.entityList map)
                renderEntity
            ]
        ]
