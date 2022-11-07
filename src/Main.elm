module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Maps
import Dict
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Player exposing (Player)
import Svg exposing (Svg)
import Tile exposing (Entity(..), Terrain(..), Tile(..))
import View



-- MODEL


type MapTransition
    = Enter Int String
    | Leave Int String String


type alias Model =
    { maps : List (HexMap Tile)
    , mapTransition : MapTransition
    , player : Player
    , selectedPoint : Point
    , renderConfig : RenderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Content.Maps.testMap
        , Content.Maps.testMap3
        ]
        (Enter transitionTime Content.Maps.testMap.name)
        (Player.new ( 0, 0, 0 ) '🐼')
        ( 0, 0, 0 )
        Render.initRenderConfig
    , Cmd.none
    )



-- MAP TRANSITION


transitionTime : Int
transitionTime =
    50


getMap : String -> List (HexMap Tile) -> Maybe (HexMap Tile)
getMap name maps =
    maps |> List.filter (\m -> m.name == name) |> List.head


currentMap : Model -> HexMap Tile
currentMap model =
    (case model.mapTransition of
        Enter _ map ->
            getMap map model.maps

        Leave _ from _ ->
            getMap from model.maps
    )
        |> Maybe.withDefault Content.Maps.errorMap


currentMapName : Model -> String
currentMapName model =
    (case model.mapTransition of
        Enter _ map ->
            Just map

        Leave _ from _ ->
            Just from
    )
        |> Maybe.withDefault "Error"


tickMapTransition : Int -> MapTransition -> MapTransition
tickMapTransition dt transition =
    case transition of
        Enter dur map ->
            Enter (max 0 (dur - dt)) map

        Leave dur from to ->
            if dur <= 0 then
                Enter transitionTime to

            else
                Leave (max 0 (dur - dt)) from to


resetPlayerPosition : MapTransition -> Player -> Player
resetPlayerPosition transition player =
    case transition of
        Leave 0 _ _ ->
            { player | position = ( 0, 0, 0 ) } |> Player.stop

        _ ->
            player



-- UPDATE


playerReadyToInteract : Player -> Point -> Bool
playerReadyToInteract player point =
    Point.distance point player.position == 1 && Player.isIdle player


type Msg
    = Tick Int
    | MapTransition String
    | ClickHex Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | mapTransition = tickMapTransition dt model.mapTransition
                , player =
                    model.player
                        |> resetPlayerPosition model.mapTransition
                        |> Player.playerMove
                        |> Player.playerCooldown dt
                , renderConfig =
                    if playerReadyToInteract model.player model.selectedPoint then
                        Render.withHexFocus model.selectedPoint model.renderConfig

                    else
                        Render.withHexFocus model.player.position model.renderConfig
              }
            , Cmd.none
            )

        MapTransition destination ->
            ( { model
                | mapTransition = Leave transitionTime (currentMapName model) destination
                , selectedPoint = ( 0, 0, 0 )
              }
            , Cmd.none
            )

        ClickHex point ->
            let
                newPlayer =
                    case Dict.get point (currentMap model).grid of
                        Just (Terrain _) ->
                            Player.playerpath (Tile.isWalkable <| currentMap model) point model.player

                        Just (TerrainEntity _ _) ->
                            Player.playerpathAdjacent (Tile.isWalkable <| currentMap model) point model.player

                        _ ->
                            model.player
            in
            ( { model
                | player =
                    newPlayer
                , selectedPoint =
                    if Player.hasPath newPlayer then
                        point

                    else
                        model.selectedPoint
              }
            , Cmd.none
            )



-- VIEW


maybeViewInteractions : Model -> List (Svg Msg)
maybeViewInteractions model =
    case Tile.getEntity model.selectedPoint (currentMap model) of
        Just e ->
            if playerReadyToInteract model.player model.selectedPoint then
                [ View.viewEntityInteractions MapTransition ( model.selectedPoint, e ) ]

            else
                []

        Nothing ->
            []


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , Render.renderMap model.renderConfig
            (currentMap model)
            (View.viewTile model.selectedPoint ClickHex)
            (View.viewPlayer model.player :: maybeViewInteractions model)
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
