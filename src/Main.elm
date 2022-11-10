module Main exposing (MapTransition, Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Maps
import Dict
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Player exposing (Player)
import Tile exposing (Tile(..))
import View



-- MODEL


type MapTransition
    = Enter Int String
    | Leave Int String String


type alias Model =
    { maps : List (HexMap Tile)
    , mapTransition : MapTransition
    , player : Player
    , selectedPoint : Maybe Point
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
        Nothing
        (Render.initRenderConfig |> Render.withZoom 1.2)
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
    case model.mapTransition of
        Enter _ map ->
            map

        Leave _ from _ ->
            from


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


type Msg
    = Tick Int
    | MapTransition String
    | ClickHex Point
    | CloseModal


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
                    model.selectedPoint
                        |> Maybe.map
                            (\p ->
                                if Player.readyToInteract model.player p then
                                    Render.withHexFocus p model.renderConfig

                                else
                                    Render.withHexFocus model.player.position model.renderConfig
                            )
                        |> Maybe.withDefault model.renderConfig
              }
            , Cmd.none
            )

        MapTransition destination ->
            ( { model
                | mapTransition = Leave transitionTime (currentMapName model) destination
                , selectedPoint = Nothing
              }
            , Cmd.none
            )

        ClickHex point ->
            let
                newPlayer : Player
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
                        Just point

                    else
                        Nothing
              }
            , Cmd.none
            )

        CloseModal ->
            ( { model
                | selectedPoint = Nothing
                , renderConfig = Render.withHexFocus model.player.position model.renderConfig
              }
            , Cmd.none
            )



-- VIEW


viewEntityModal : Model -> List (Html Msg)
viewEntityModal model =
    model.selectedPoint
        |> Maybe.map
            (\p ->
                case Tile.getEntity p (currentMap model) of
                    Just e ->
                        if Player.readyToInteract model.player p then
                            [ View.entityModal CloseModal e ]

                        else
                            []

                    Nothing ->
                        []
            )
        |> Maybe.withDefault []


view : Model -> Html Msg
view model =
    main_ []
        ([ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
         , Render.renderMap model.renderConfig
            (currentMap model)
            (View.viewTile model.player.position model.selectedPoint ClickHex)
            [ View.viewPlayer model.player ]
         ]
            ++ viewEntityModal model
        )



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
