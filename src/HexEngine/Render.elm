module HexEngine.Render exposing
    ( RenderConfig
    , cornersToString
    , cornersToString2
    , fancyHexCorners
    , fancyHexCorners2
    , initRenderConfig
    , pointAdd
    , pointToPixel
    , renderGrid
    , renderGrid2
    , renderTileEntityMap
    , withCameraPosition
    , withHexFocus
    , withZoom
    )

import Dict
import HexEngine.HexEntityMap exposing (HexEntityMap)
import HexEngine.HexMap exposing (HexMap)
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
        pos =
            point |> pointToPixel
    in
    config |> withCameraPosition pos


withZoom : Float -> RenderConfig -> RenderConfig
withZoom zoom config =
    { config | zoom = zoom }



---- GENERAL STUFF ----


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


cornersToString : List ( Float, Float ) -> String
cornersToString points =
    let
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    List.map tupleToString points |> List.intersperse " " |> String.concat


cornersToString2 : HexCorners -> String
cornersToString2 points =
    let
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


{-| Calculate hex corners in screen coordinates
-}
fancyHexCorners2 : HexCorners
fancyHexCorners2 =
    let
        angleRad cornerNumber =
            degrees (60 * cornerNumber |> toFloat)

        corner cornerNumber =
            ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
            , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
            )
    in
    HexCorners
        (corner 0)
        (corner 1)
        (corner 2)
        (corner 3)
        (corner 4)
        (corner 5)


{-| Calculate hex corners in screen coordinates
-}
fancyHexCorners : List ( Float, Float )
fancyHexCorners =
    let
        angleRad cornerNumber =
            degrees (60 * cornerNumber |> toFloat)

        corner cornerNumber =
            ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
            , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
            )
    in
    [ corner 0
    , corner 1
    , corner 2
    , corner 3
    , corner 4
    , corner 5
    ]


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


renderLayer : HexMap tile -> (( Point, tile ) -> Svg msg) -> Svg msg
renderLayer grid renderTile =
    Svg.Keyed.node "g"
        [ Svg.Attributes.class "layer" ]
        -- sort by y position and render
        (grid
            |> Dict.toList
            |> List.sortBy (Tuple.first >> pointToYpos)
            |> List.map (keyedViewHex renderTile)
        )


renderGrid : RenderConfig -> HexMap tile -> (( Point, tile ) -> Svg msg) -> List (Svg msg) -> Svg msg
renderGrid config map renderTile extras =
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
            (Svg.Lazy.lazy2 renderLayer map renderTile :: extras)
        ]


renderTileEntityMap : RenderConfig -> HexEntityMap tile entity -> (( Point, tile ) -> Svg msg) -> (( Point, entity ) -> Svg msg) -> List (Svg msg) -> Svg msg
renderTileEntityMap config map renderTile renderEntity extras =
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
            ([ Svg.Lazy.lazy2 renderLayer map.tiles renderTile
             , Svg.Lazy.lazy2 renderLayer map.entities renderEntity
             ]
                ++ extras
            )
        ]


renderGrid2 : RenderConfig -> HexMap tile1 -> (( Point, tile1 ) -> Svg msg) -> HexMap tile2 -> (( Point, tile2 ) -> Svg msg) -> Svg msg
renderGrid2 config map1 renderTile1 map2 renderTile2 =
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
            [ renderLayer map1 renderTile1
            , renderLayer map2 renderTile2
            ]
        ]
