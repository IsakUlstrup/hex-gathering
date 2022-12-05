module HexEngine.Render exposing
    ( HexCorners
    , RenderConfig
    , cornerListToString
    , cornersToString
    , generateHexCorners
    , initRenderConfig
    , pointAdd
    , viewWorld2
    , withEntityFocus
    , withPlayerFocus
    , withZoom
    )

import HexEngine.Entity as Entity exposing (Entity, EntityState(..), WorldPosition)
import HexEngine.Point as Point exposing (Point)
import HexEngine.World as World exposing (World)
import Svg exposing (Attribute, Svg, g, svg)
import Svg.Attributes
import Svg.Keyed
import Svg.Lazy



---- CONFIG BUILDER ----


type alias RenderConfig =
    { position : Point
    , zoom : Float
    }


initRenderConfig : RenderConfig
initRenderConfig =
    RenderConfig ( 0, 0, 0 ) 1


{-| move camera to focus on point
-}
withHexFocus : Point -> RenderConfig -> RenderConfig
withHexFocus point config =
    { config | position = point }


withEntityFocus : WorldPosition -> RenderConfig -> RenderConfig
withEntityFocus position config =
    config |> withHexFocus (Point.add position.map position.local)


withPlayerFocus : World tileData entityData -> RenderConfig -> RenderConfig
withPlayerFocus world config =
    config |> withEntityFocus (World.getPlayerPosition world)


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


yPixelPosition : Point -> Float
yPixelPosition position =
    pointToPixel position |> Tuple.second


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


{-| Calculate hex corners in screen coordinates
-}
generateHexCorners : HexCorners
generateHexCorners =
    let
        corner cornerNumber =
            ( hexSize * cos (degrees <| 60 * cornerNumber)
            , hexSize * sin (degrees <| 60 * cornerNumber)
            )
    in
    HexCorners
        (corner 0)
        (corner 1)
        (corner 2)
        (corner 3)
        (corner 4)
        (corner 5)



-- RENDER


translate2 : Float -> Float -> String
translate2 x y =
    "transform: translate("
        ++ String.fromFloat x
        ++ "px, "
        ++ String.fromFloat y
        ++ "px);"


translatePoint : Point -> Attribute msg
translatePoint position =
    let
        ( x, y ) =
            pointToPixel position
    in
    Svg.Attributes.style <| translate2 x y


renderTile : (( Point, a ) -> Svg msg) -> ( Point, a ) -> Svg msg
renderTile renderFunc ( point, t ) =
    Svg.g
        [ Svg.Attributes.class "tile"
        , translatePoint point
        ]
        [ renderFunc ( point, t ) ]


keyedViewTile : (( Point, a ) -> String) -> (( Point, a ) -> Svg msg) -> ( Point, a ) -> ( String, Svg msg )
keyedViewTile keyFunc renderFunc entity =
    ( keyFunc entity
    , Svg.Lazy.lazy (renderTile renderFunc) entity
    )



-- renderWorldHex : (( Point, tile ) -> Svg msg) -> ( WorldPosition, tile ) -> Svg msg
-- renderWorldHex renderTile ( worldPos, t ) =
--     let
--         ( x, y ) =
--             pointToPixel (Point.add worldPos.map worldPos.local)
--     in
--     g [ Svg.Attributes.class "point", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat (x - hexSize / 2) ++ "px, " ++ String.fromFloat (y - hexSize / 2) ++ "px);") ]
--         [ renderTile ( worldPos.local, t ) ]
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



-- layer : Bool -> List (Attribute msg) -> (( WorldPosition, a ) -> String) -> (( Point, a ) -> Svg msg) -> List ( WorldPosition, a ) -> Svg msg
-- layer sort attributes keyFunc renderTile tiles =
--     let
--         keyedViewTile : ( WorldPosition, a ) -> ( String, Svg msg )
--         keyedViewTile entity =
--             ( keyFunc entity
--             , Svg.Lazy.lazy (renderWorldHex renderTile) entity
--             )
--     in
--     Svg.Keyed.node "g"
--         (Svg.Attributes.class "layer" :: attributes)
--         (tiles
--             |> (if sort then
--                     List.sortBy (Tuple.first >> yPixelPosition)
--                 else
--                     identity
--                )
--             |> List.map keyedViewTile
--         )


layer2 : Bool -> List (Attribute msg) -> (( Point, a ) -> String) -> (( Point, a ) -> Svg msg) -> List ( Point, a ) -> Svg msg
layer2 sort attributes keyFunc renderFunc tiles =
    Svg.Keyed.node "g"
        (Svg.Attributes.class "layer" :: attributes)
        (tiles
            |> (if sort then
                    List.sortBy (Tuple.first >> yPixelPosition)

                else
                    identity
               )
            |> List.map (keyedViewTile keyFunc renderFunc)
        )


renderMap :
    (( Point, tileData ) -> Svg msg)
    -> Point
    -> List ( Point, tileData )
    -> Svg msg
renderMap renderTileFunc mapPosition tiles =
    g
        [ Svg.Attributes.class "map"
        , translatePoint mapPosition
        ]
        [ layer2 True [ Svg.Attributes.class "terrain" ] (Tuple.first >> Point.toString) renderTileFunc tiles

        -- , layer2 False [ Svg.Attributes.class "entities" ] (Tuple.second >> .id >> String.fromInt) renderEntity entities
        ]


customSvg : RenderConfig -> List (Svg msg) -> Svg msg
customSvg config children =
    let
        ( camX, camY ) =
            config.position |> pointToPixel
    in
    svg
        [ Svg.Attributes.viewBox
            ([ -50, -50, 100, 100 ]
                |> List.map String.fromFloat
                |> List.intersperse " "
                |> String.concat
            )
        , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
        ]
        [ Svg.g
            [ Svg.Attributes.style
                ("transform: translate("
                    ++ String.fromFloat -(camX * config.zoom)
                    ++ "px, "
                    ++ String.fromFloat -(camY * config.zoom)
                    ++ "px) scale("
                    ++ String.fromFloat config.zoom
                    ++ ");"
                )
            , Svg.Attributes.class "camera"
            ]
            children
        ]


renderEntity :
    (( Point, Entity entityData )
     -> Svg msg
    )
    -> List Point
    -> Entity entityData
    -> Maybe (Svg msg)
renderEntity renderFunc targetMaps entity =
    let
        position =
            Entity.getPosition entity
    in
    if List.member position.map targetMaps then
        Just <|
            Svg.g
                [ Svg.Attributes.class "entity"
                , Svg.Attributes.class (Entity.stateString entity)
                , translatePoint (Point.add position.map position.local)
                ]
                [ renderFunc ( Point.add position.map position.local, entity ) ]

    else
        Nothing


viewWorld2 :
    RenderConfig
    -> World tileData entityData
    -> (( Point, tileData ) -> Svg msg)
    -> (( Point, Entity entityData ) -> Svg msg)
    -> Svg msg
viewWorld2 config world tileRenderFunc entityRenderFunc =
    customSvg config <|
        case (World.getPlayer world).state of
            MapTransitionCharge _ from to ->
                [ World.mapEntityGrid from.map (renderMap tileRenderFunc) world
                , World.mapEntityGrid to.map (renderMap tileRenderFunc) world
                , Svg.g [ Svg.Attributes.class "entities" ] (World.filterMapEntities (renderEntity entityRenderFunc [ from.map, to.map ]) world)
                ]

            MapTransitionMove _ from to ->
                [ World.mapEntityGrid from.map (renderMap tileRenderFunc) world
                , World.mapEntityGrid to.map (renderMap tileRenderFunc) world
                , Svg.g [ Svg.Attributes.class "entities" ] (World.filterMapEntities (renderEntity entityRenderFunc [ from.map, to.map ]) world)
                ]

            _ ->
                [ World.mapEntityGrid (World.getPlayerPosition world).map (renderMap tileRenderFunc) world
                , Svg.g [ Svg.Attributes.class "entities" ] (World.filterMapEntities (renderEntity entityRenderFunc [ (World.getPlayerPosition world).map ]) world)
                ]



-- viewWorld :
--     RenderConfig
--     -> World tileData entityData
--     -> (( Point, tileData ) -> Svg msg)
--     -> (( Point, Entity entityData ) -> Svg msg)
--     -> Svg msg
-- viewWorld config world tileRenderFunc entityRenderFunc =
--     svg
--         [ Svg.Attributes.viewBox ([ -50, -50, 100, 100 ] |> List.map String.fromFloat |> List.intersperse " " |> String.concat)
--         , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
--         ]
--         [ Svg.g
--             [ Svg.Attributes.style
--                 ("transform: translate("
--                     ++ String.fromFloat -(config.cameraX * config.zoom)
--                     ++ "px, "
--                     ++ String.fromFloat -(config.cameraY * config.zoom)
--                     ++ "px) scale("
--                     ++ String.fromFloat config.zoom
--                     ++ ");"
--                 )
--             , Svg.Attributes.class "camera"
--             ]
--             [ world
--                 |> World.mapCurrentGrid
--                     (layer True [ Svg.Attributes.class "terrain" ] (Tuple.first >> Entity.worldPositionToString) tileRenderFunc)
--             , world
--                 |> World.mapCurrentEntities
--                     (layer False [ Svg.Attributes.class "entities" ] (Tuple.second >> .id >> String.fromInt) entityRenderFunc)
--             ]
--         ]
