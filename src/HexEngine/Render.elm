module HexEngine.Render exposing
    ( HexCorners
    , RenderConfig
    , cornerListToString
    , cornersToString
    , hardcodedPoints
    , initRenderConfig
    , pointAdd
    , viewWorld
    , withEntityFocus
    , withHexFocus
    , withZoom
    )

import HexEngine.Entity exposing (Entity, WorldPosition)
import HexEngine.Point as Point exposing (Point)
import HexEngine.World as World exposing (World)
import Svg exposing (Attribute, Svg, g, svg)
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


withEntityFocus : WorldPosition -> RenderConfig -> RenderConfig
withEntityFocus position config =
    let
        pos : ( Float, Float )
        pos =
            Point.add position.map position.local |> pointToPixel
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



-- pointToYpos : Point -> Float
-- pointToYpos point =
--     pointToPixel point |> Tuple.second


yPixelPosition : WorldPosition -> Float
yPixelPosition position =
    pointToPixel position.local |> Tuple.second


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
-- renderHex : (( Point, tile ) -> Svg msg) -> ( Point, tile ) -> Svg msg
-- renderHex renderTile ( point, t ) =
--     let
--         ( x, y ) =
--             pointToPixel point
--     in
--     g [ Svg.Attributes.class "point", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat (x - hexSize / 2) ++ "px, " ++ String.fromFloat (y - hexSize / 2) ++ "px);") ]
--         [ renderTile ( point, t ) ]


renderWorldHex : (( Point, tile ) -> Svg msg) -> ( WorldPosition, tile ) -> Svg msg
renderWorldHex renderTile ( worldPos, t ) =
    let
        ( x, y ) =
            pointToPixel (Point.add worldPos.map worldPos.local)
    in
    g [ Svg.Attributes.class "point", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat (x - hexSize / 2) ++ "px, " ++ String.fromFloat (y - hexSize / 2) ++ "px);") ]
        [ renderTile ( worldPos.local, t ) ]



-- EXPERIMENTAL
-- sortedLayer : List (Attribute msg) -> (( Point, a ) -> String) -> (( Point, a ) -> Svg msg) -> List ( Point, a ) -> Svg msg
-- sortedLayer attributes keyFunc renderTile tiles =
--     let
--         keyedViewTile : ( Point, a ) -> ( String, Svg msg )
--         keyedViewTile entity =
--             ( keyFunc entity
--             , Svg.Lazy.lazy (renderHex renderTile) entity
--             )
--     in
--     Svg.Keyed.node "g"
--         (Svg.Attributes.class "layer" :: attributes)
--         -- sort by y position and render
--         (tiles
--             |> List.sortBy (Tuple.first >> pointToYpos)
--             |> List.map keyedViewTile
--         )


{-| Render a keyed layer

sort: If true sort tiles by y value to prevent tiles from rendering on top of each other

attributes: a list of svg attributes that are set on the Svg.g element

keyFunc: a function that returns tile element key (should be unique)

renderTile: tile render function

tiles: a list of tiles

-}
layer : Bool -> List (Attribute msg) -> (( WorldPosition, a ) -> String) -> (( Point, a ) -> Svg msg) -> List ( WorldPosition, a ) -> Svg msg
layer sort attributes keyFunc renderTile tiles =
    let
        keyedViewTile : ( WorldPosition, a ) -> ( String, Svg msg )
        keyedViewTile entity =
            ( keyFunc entity
            , Svg.Lazy.lazy (renderWorldHex renderTile) entity
            )
    in
    Svg.Keyed.node "g"
        (Svg.Attributes.class "layer" :: attributes)
        (tiles
            |> (if sort then
                    List.sortBy (Tuple.first >> yPixelPosition)

                else
                    identity
               )
            |> List.map keyedViewTile
        )


viewWorld :
    RenderConfig
    -> World tileData entityData
    -> (( Point, tileData ) -> Svg msg)
    -> (( Point, Entity entityData ) -> Svg msg)
    -> Svg msg
viewWorld config world tileRenderFunc entityRenderFunc =
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
            [ world
                |> World.mapCurrentGrid
                    (layer True [ Svg.Attributes.class "terrain" ] (Tuple.first >> .local >> Point.toString) tileRenderFunc)
            , world
                |> World.mapCurrentEntities
                    (layer False [ Svg.Attributes.class "entities" ] (Tuple.second >> .id >> String.fromInt) entityRenderFunc)
            ]
        ]
