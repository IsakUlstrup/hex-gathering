module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Maps
import Dict
import HexEngine.HexMap as HexMap exposing (HexMap)
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Player exposing (Player)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Tile exposing (Tile(..))



-- TODO
-- Extract view
-- Make EntityMap module that combines map and entities
-- MODEL


isWalkable : HexMap Tile -> Point -> Bool
isWalkable map point =
    case Dict.get point map of
        Just tile ->
            case tile of
                Medium ->
                    True

                _ ->
                    False

        Nothing ->
            False


type alias Model =
    { map : HexMap Tile
    , player : Player
    , tree : Player
    , renderConfig : RenderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Maps.testMap
        (Player.new ( 0, 0, 0 ) '🐼')
        (Player.new ( 6, -3, -3 ) '🌲')
        Render.initRenderConfig
    , Cmd.none
    )



-- UPDATE


type Msg
    = FocusTile Point
    | Tick Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusTile point ->
            ( { model
                | player = Player.playerpath (isWalkable model.map) point model.player
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | player =
                    model.player
                        |> Player.playerMove
                        |> Player.playerCooldown dt
                , renderConfig = Render.withHexFocus model.player.position model.renderConfig
              }
            , Cmd.none
            )



-- VIEW


viewTile : ( Point, Tile ) -> Svg Msg
viewTile ( point, tile ) =
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
            Render.fancyHexCorners2

        columnHeight =
            case tile of
                Low ->
                    0.5

                Medium ->
                    2

                High ->
                    4

        events =
            case tile of
                Medium ->
                    [ Svg.Events.onClick <| FocusTile point
                    ]

                _ ->
                    []

        animationDelayMultiplier =
            70
    in
    Svg.g
        ([ Svg.Attributes.class "hex"
         , Svg.Attributes.class tileType
         , Svg.Attributes.style
            ("animation-delay: "
                ++ (Point.distanceFloat ( 0, 0, 0 ) point
                        |> (*) animationDelayMultiplier
                        |> String.fromFloat
                   )
                ++ "ms"
            )
         ]
            ++ events
        )
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


viewPlayer : Player -> Svg Msg
viewPlayer player =
    let
        ( x, y ) =
            Render.pointToPixel player.position
    in
    Svg.g [ Svg.Attributes.class (Player.moveStateString player), Svg.Attributes.class "player", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px);") ]
        [ Svg.text_ [ Svg.Attributes.class "player-icon" ] [ Svg.text (player.icon |> String.fromChar) ]
        ]


viewHighlight : Point -> Svg msg
viewHighlight point =
    let
        ( x, y ) =
            Render.pointToPixel point
    in
    Svg.g [ Svg.Attributes.class "highlight", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px);") ]
        [ Svg.circle [ Svg.Attributes.class "move-target", Svg.Attributes.r "1", Svg.Attributes.fill "white", Svg.Attributes.fillOpacity "0.7" ] [] ]


viewPlayerMoveTarget : Player -> Svg msg
viewPlayerMoveTarget player =
    Player.moveTarget player
        |> viewHighlight


view : Model -> Html Msg
view model =
    main_ []
        [ Render.renderGrid model.renderConfig
            model.map
            viewTile
            [ viewPlayerMoveTarget model.player
            , viewPlayer model.tree
            , viewPlayer model.player
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (round >> Tick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
