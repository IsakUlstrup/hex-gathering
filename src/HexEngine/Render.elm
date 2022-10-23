module HexEngine.Render exposing
    ( RenderConfig
    , cornersToString
    , cornersToString2
    , fancyHexCorners
    , fancyHexCorners2
    , initRenderConfig
    , pointAdd
    , renderGrid
    , withCameraPosition
    , withFlatTop
    , withHexFocus
    , withZoom
    )

import Dict
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
    , flatTop : Bool
    }


initRenderConfig : RenderConfig
initRenderConfig =
    RenderConfig 0 0 1 True


withCameraPosition : ( Float, Float ) -> RenderConfig -> RenderConfig
withCameraPosition ( x, y ) config =
    { config | cameraX = x, cameraY = y }


{-| move camera to focus on point
-}
withHexFocus : Point -> RenderConfig -> RenderConfig
withHexFocus point config =
    let
        pos =
            point |> pointToPixel config.flatTop
    in
    config |> withCameraPosition pos


withZoom : Float -> RenderConfig -> RenderConfig
withZoom zoom config =
    { config | zoom = zoom }


withFlatTop : Bool -> RenderConfig -> RenderConfig
withFlatTop flat config =
    { config | flatTop = flat }



---- GENERAL STUFF ----


{-| Hex size constant
-}
hexSize : Float
hexSize =
    5


{-| Get the center of a given point in screen coordinates
-}
pointToPixel : Bool -> Point -> ( Float, Float )
pointToPixel flatTop point =
    let
        ( q, r ) =
            Point.toAxial point
    in
    if flatTop then
        ( hexSize * (3 / 2 * toFloat q)
        , hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r)
        )

    else
        ( hexSize * (sqrt 3 * toFloat q + sqrt 3 / 2 * toFloat r)
        , hexSize * (3 / 2 * toFloat r)
        )


pointToYpos : Bool -> Point -> Float
pointToYpos flatTop point =
    pointToPixel flatTop point |> Tuple.second


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
fancyHexCorners2 : RenderConfig -> HexCorners
fancyHexCorners2 config =
    let
        angleRad cornerNumber =
            if config.flatTop then
                degrees (60 * cornerNumber |> toFloat)

            else
                degrees (60 * cornerNumber - 30 |> toFloat)

        corner cornerNumber =
            if config.flatTop then
                ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
                , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
                )

            else
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
fancyHexCorners : RenderConfig -> List ( Float, Float )
fancyHexCorners config =
    let
        angleRad cornerNumber =
            if config.flatTop then
                degrees (60 * cornerNumber |> toFloat)

            else
                degrees (60 * cornerNumber - 30 |> toFloat)

        corner cornerNumber =
            if config.flatTop then
                ( (hexSize / 2) + hexSize * cos (angleRad cornerNumber)
                , (hexSize / 2) + hexSize * sin (angleRad cornerNumber)
                )

            else
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


renderHex : RenderConfig -> (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> Svg msg
renderHex config renderTile ( point, t ) =
    let
        ( x, y ) =
            pointToPixel config.flatTop point
    in
    g [ Svg.Attributes.style ("transform: translate3d(" ++ String.fromFloat (x - hexSize / 2) ++ "px, " ++ String.fromFloat (y - hexSize / 2) ++ "px, 0px) scale(0.97);") ]
        [ renderTile ( point, t ) ]


keyedViewHex : RenderConfig -> (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> ( String, Svg msg )
keyedViewHex config renderTile tile =
    ( Point.toString (Tuple.first tile)
    , Svg.Lazy.lazy (renderHex config renderTile) tile
    )



-- <defs>
--     <linearGradient id="fadeGrad" y2="1" x2="0">
--       <stop offset="0.5" stop-color="white" stop-opacity="0"/>
--       <stop offset="1" stop-color="white" stop-opacity=".5"/>
--     </linearGradient>
--     <mask id="fade" maskContentUnits="objectBoundingBox">
--       <rect width="1" height="1" fill="url(#fadeGrad)"/>
--     </mask>
--   </defs>


fadeMask : Svg msg
fadeMask =
    Svg.defs []
        [ Svg.linearGradient
            [ Svg.Attributes.id "fadeGradient"
            , Svg.Attributes.y2 "1"
            , Svg.Attributes.x2 "0"
            ]
            [ Svg.stop [ Svg.Attributes.offset "0.7", Svg.Attributes.stopColor "white", Svg.Attributes.stopOpacity "1" ] []
            , Svg.stop [ Svg.Attributes.offset "1", Svg.Attributes.stopColor "white", Svg.Attributes.stopOpacity "0" ] []
            ]
        , Svg.mask
            [ Svg.Attributes.id "fadeMask"
            , Svg.Attributes.maskContentUnits "objectBoundingBox"
            ]
            [ Svg.polygon
                [ Svg.Attributes.points "0,0 1,0 1,0.75 0.75,1 0.25,1 0,0.75"

                -- , Svg.Attributes.fill "url(#fadeGradient)"
                , Svg.Attributes.fill "white"
                ]
                []
            ]
        ]


renderGrid : RenderConfig -> HexMap tile -> (( Point, tile ) -> Svg msg) -> Svg msg
renderGrid config map renderTile =
    svg
        [ Svg.Attributes.viewBox ([ -50, -50, 100, 100 ] |> List.map String.fromFloat |> List.intersperse " " |> String.concat)
        , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
        ]
        [ fadeMask
        , Svg.Keyed.node "g"
            [ Svg.Attributes.style
                ("transform: translate("
                    ++ String.fromFloat -(config.cameraX * config.zoom)
                    ++ "px, "
                    ++ String.fromFloat -(config.cameraY * config.zoom)
                    ++ "px) scale("
                    ++ String.fromFloat config.zoom
                    ++ ");"
                )
            , Svg.Attributes.id "root"
            ]
            -- (mapHexes (keyedViewHex config renderTile) map)
            -- sort by y position and render
            (map
                |> Dict.toList
                |> List.sortBy (Tuple.first >> pointToYpos True)
                |> List.map (keyedViewHex config renderTile)
            )
        ]
