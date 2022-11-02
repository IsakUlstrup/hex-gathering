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
    , selectedEntity : Maybe ( Point, Entity )
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
        Render.initRenderConfig
    , Cmd.none
    )



-- MAP TRANSITION


transitionTime : Int
transitionTime =
    500


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



-- MAP INTERACTION


getEntity : Point -> HexMap Tile -> Maybe Entity
getEntity position map =
    case Dict.get position map.grid of
        Just (TerrainEntity _ entity) ->
            Just entity

        _ ->
            Nothing



-- UPDATE


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
                , renderConfig = Render.withHexFocus model.player.position model.renderConfig
              }
            , Cmd.none
            )

        MapTransition destination ->
            ( { model
                | mapTransition = Leave 500 (currentMapName model) destination
                , selectedEntity = Nothing
              }
            , Cmd.none
            )

        ClickHex point ->
            ( { model
                | player = Player.playerpath (Tile.isWalkable <| currentMap model) point model.player
                , selectedEntity =
                    if Point.distance model.player.position point == 1 then
                        getEntity point (currentMap model) |> Maybe.map (Tuple.pair point)

                    else
                        Nothing
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , Render.renderMap model.renderConfig
            (currentMap model)
            (View.viewTile ClickHex)
            [ View.viewPlayerMoveTarget model.player
            , View.viewPlayer model.player
            , View.viewEntityInteractions MapTransition model.selectedEntity
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
