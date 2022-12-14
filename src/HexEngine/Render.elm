module HexEngine.Render exposing
    ( HexCorners
    , RenderConfig
    , cornerListToPoints
    , cornersToPoints
    , generateHexCorners
    , initRenderConfig
    , pointAdd
    , viewWorld
    , withEntityFocus
    , withPlayerFocus
    , withZoom
    )

import HexEngine.Entity as Entity exposing (Entity, EntityState(..), WorldPosition)
import HexEngine.Point as Point exposing (Point)
import HexEngine.World as World exposing (World)
import Svg exposing (Attribute, Svg, svg)
import Svg.Attributes
import Svg.Keyed
import Svg.Lazy



---- CONFIG BUILDER ----


{-| Holds renderer config values
-}
type alias RenderConfig =
    { position : Point
    , zoom : Float
    }


{-| Default render config
-}
initRenderConfig : RenderConfig
initRenderConfig =
    RenderConfig ( 0, 0, 0 ) 1


{-| move camera to focus on point
-}
withHexFocus : Point -> RenderConfig -> RenderConfig
withHexFocus point config =
    { config | position = point }


{-| Focus camera on entity
-}
withEntityFocus : WorldPosition -> RenderConfig -> RenderConfig
withEntityFocus position config =
    config |> withHexFocus (Point.add position.map position.local)


{-| Focus camera on player
-}
withPlayerFocus : World tileData entityData -> RenderConfig -> RenderConfig
withPlayerFocus world config =
    config |> withEntityFocus (World.getPlayerPosition world)


{-| Set camera zoom
-}
withZoom : Float -> RenderConfig -> RenderConfig
withZoom zoom config =
    { config | zoom = zoom }



-- HELPERS


{-| Hex size constant

If you want to change hex size, use WithZoom instead

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


{-| Get point y position in pixels
-}
yPixelPosition : Point -> Float
yPixelPosition position =
    pointToPixel position |> Tuple.second


{-| Convert a list of floats to a Svg points attribute
-}
cornerListToPoints : List ( Float, Float ) -> Attribute msg
cornerListToPoints points =
    let
        tupleToString : ( Float, Float ) -> String
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    List.map tupleToString points |> List.intersperse " " |> String.concat |> Svg.Attributes.points


{-| Transform Hex Corners to Svg points attribute
-}
cornersToPoints : HexCorners -> Attribute msg
cornersToPoints points =
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
        |> Svg.Attributes.points


{-| Represents the points of a hexagon, staring with east and moving counter clockwise
-}
type alias HexCorners =
    { p0 : ( Float, Float )
    , p1 : ( Float, Float )
    , p2 : ( Float, Float )
    , p3 : ( Float, Float )
    , p4 : ( Float, Float )
    , p5 : ( Float, Float )
    }


{-| Add two tuples of floats together
-}
pointAdd : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
pointAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| Calculate hex corners in screen coordinates
-}
generateHexCorners : HexCorners
generateHexCorners =
    let
        corner : Float -> ( Float, Float )
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


{-| CSS transform translate attribute based on position
-}
translatePoint : Point -> Attribute msg
translatePoint position =
    let
        ( x, y ) =
            pointToPixel position
    in
    Svg.Attributes.style <|
        "transform: translate("
            ++ String.fromFloat x
            ++ "px, "
            ++ String.fromFloat y
            ++ "px);"


{-| A custom SVG element with camera support
-}
customSvg : RenderConfig -> Svg msg -> List ( String, Svg msg ) -> Svg msg
customSvg config defs children =
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
        [ defs
        , Svg.Keyed.node "g"
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


{-| Create a wrapper with correct position and render a tile with provied function
-}
renderTile : (( Point, a ) -> Svg msg) -> ( Point, a ) -> Svg msg
renderTile renderFunc ( point, t ) =
    Svg.g
        [ Svg.Attributes.class "tile"
        , translatePoint point
        ]
        [ renderFunc ( point, t ) ]


{-| Keyed and lazy tile render
-}
viewKeyedTile : (( Point, a ) -> String) -> (( Point, a ) -> Svg msg) -> ( Point, a ) -> ( String, Svg msg )
viewKeyedTile keyFunc renderFunc entity =
    ( keyFunc entity
    , Svg.Lazy.lazy (renderTile renderFunc) entity
    )


{-| If current grid position matches and of the target maps, render grid
-}
viewKeyedGrid : (( Point, tileData ) -> Svg msg) -> List Point -> Point -> List ( Point, tileData ) -> Maybe ( String, Svg msg )
viewKeyedGrid renderFunc targetMaps mapPosition grid =
    if List.member mapPosition targetMaps then
        Just <|
            ( Point.toString mapPosition
            , Svg.Keyed.node "g"
                [ Svg.Attributes.class "map"
                , translatePoint mapPosition
                ]
                (grid
                    |> List.sortBy (Tuple.first >> yPixelPosition)
                    |> List.map (viewKeyedTile (Tuple.first >> Point.toString) renderFunc)
                )
            )

    else
        Nothing


{-| If entity map position is in target maps, render entity
-}
viewKeyedEntity :
    (( Point, Entity entityData )
     -> Svg msg
    )
    -> List Point
    -> Entity entityData
    -> Maybe ( String, Svg msg )
viewKeyedEntity renderFunc targetMaps entity =
    let
        position : WorldPosition
        position =
            Entity.getPosition entity
    in
    if List.member position.map targetMaps then
        Just <|
            ( entity.id |> String.fromInt
            , Svg.g
                [ Svg.Attributes.class "entity"
                , Svg.Attributes.class (Entity.stateString entity)
                , translatePoint (Point.add position.map position.local)
                ]
                [ renderFunc ( position.local, entity ) ]
            )

    else
        Nothing


viewCurrentMaps : (( Point, tileData ) -> Svg msg) -> World tileData entityData -> List Point -> Svg msg
viewCurrentMaps tileRenderFunc world maps =
    let
        _ =
            Debug.log "render" maps
    in
    Svg.Keyed.node "g" [ Svg.Attributes.class "maps" ] (World.filterMapGrids (viewKeyedGrid tileRenderFunc maps) world)


{-| Render a world
-}
viewWorld :
    RenderConfig
    -> Svg msg
    -> World tileData entityData
    -> (( Point, tileData ) -> Svg msg)
    -> (( Point, Entity entityData ) -> Svg msg)
    -> List Point
    -> Svg msg
viewWorld config defs world tileRenderFunc entityRenderFunc maps =
    customSvg config
        defs
        [ ( "maps"
          , Svg.Lazy.lazy (viewCurrentMaps tileRenderFunc world) maps
          )
        , ( "entities"
          , Svg.Keyed.node "g" [ Svg.Attributes.class "entities" ] (World.filterMapEntities (viewKeyedEntity entityRenderFunc maps) world)
          )
        ]
