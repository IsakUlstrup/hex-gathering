module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Dict
import HexEngine.HexMap as HexMap exposing (HexMap)
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events



-- MODEL


type Tile
    = Low
    | Medium
    | High


type alias Player =
    { position : Point
    , icon : Char
    , path : Maybe (List Point)
    , moveCooldown : Int
    }


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


playerpath : (Point -> Bool) -> Point -> Player -> Player
playerpath walkable to player =
    { player | path = Point.pathfind walkable player.position to }


playerCooldown : Int -> Player -> Player
playerCooldown dt player =
    { player | moveCooldown = max 0 (player.moveCooldown - dt) }


playerMove : Player -> Player
playerMove player =
    case player.path of
        Just (t :: ts) ->
            if player.moveCooldown <= 0 then
                { player
                    | position = t
                    , path = Just ts
                    , moveCooldown = 800
                }

            else
                player

        _ ->
            player


type alias Model =
    { map : HexMap Tile
    , player : Player
    , renderConfig : RenderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (HexMap.empty
            |> HexMap.insertReplaceHex ( ( 0, 0, 0 ), Medium )
            |> HexMap.insertReplaceHex ( ( 0, 1, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 1, 0, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 1, 1, -2 ), High )
            |> HexMap.insertReplaceHex ( ( -1, 1, 0 ), Low )
            |> HexMap.insertReplaceHex ( ( -1, 2, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( -1, 3, -2 ), Medium )
            |> HexMap.insertReplaceHex ( ( 0, 3, -3 ), Low )
            |> HexMap.insertReplaceHex ( ( 1, 2, -3 ), Low )
            |> HexMap.insertReplaceHex ( ( 1, 3, -4 ), Low )
            |> HexMap.insertReplaceHex ( ( 2, -1, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 3, -2, -1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 4, -2, -2 ), Medium )
            |> HexMap.insertReplaceHex ( ( 6, -3, -3 ), High )
            |> HexMap.insertReplaceHex ( ( 5, -2, -3 ), Medium )
            |> HexMap.insertReplaceHex ( ( -4, 2, 2 ), Medium )
            |> HexMap.insertReplaceHex ( ( -5, 3, 2 ), Medium )
            |> HexMap.insertReplaceHex ( ( -5, 2, 3 ), Medium )
            |> HexMap.insertReplaceHex ( ( -4, 3, 1 ), High )
            |> HexMap.insertReplaceHex ( ( 0, -1, 1 ), Low )
            |> HexMap.insertReplaceHex ( ( 1, -2, 1 ), Medium )
            |> HexMap.insertReplaceHex ( ( 1, -1, 0 ), Medium )
            |> HexMap.insertReplaceHex ( ( 0, -2, 2 ), Medium )
            |> HexMap.insertReplaceHex ( ( 0, -3, 3 ), Medium )
        )
        (Player ( 0, 0, 0 ) '🐼' Nothing 500)
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
                | player = playerpath (isWalkable model.map) point model.player
              }
            , Cmd.none
            )

        Tick dt ->
            let
                newPlayer =
                    model.player
                        |> playerCooldown dt
                        |> playerMove
            in
            ( { model
                | player = newPlayer
                , renderConfig = Render.withHexFocus newPlayer.position model.renderConfig
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
    Svg.g [ Svg.Attributes.class "player", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px);") ]
        [ Svg.text_ [ Svg.Attributes.class "player-icon", Svg.Attributes.x "-2.5" ] [ Svg.text (player.icon |> String.fromChar) ] ]



-- viewHighlight : Point -> Svg msg
-- viewHighlight point =
--     let
--         ( x, y ) =
--             Render.pointToPixel point
--     in
--     Svg.g [ Svg.Attributes.class "highlight", Svg.Attributes.style ("transform: translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px);") ]
--         [ Svg.text_ [ Svg.Attributes.x "-2.5" ] [ Svg.text "O" ] ]


view : Model -> Html Msg
view model =
    main_ []
        [ Render.renderGrid model.renderConfig model.map viewTile [ viewPlayer model.player ]
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
