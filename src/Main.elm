module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Maps
import Dict exposing (Dict)
import HexEngine.HexEntityMap exposing (HexEntityMap)
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Player exposing (Player)
import Tile exposing (Entity(..), Tile(..))
import View



-- MODEL


{-| determine if a given point is walkable

Only tiles that exist and are of variant Medium are walkable

Entities are not walkable

-}
isWalkable : HexEntityMap Tile Entity -> Point -> Bool
isWalkable map point =
    case ( Dict.get point map.tiles, Dict.get point map.entities ) of
        ( Just t, Nothing ) ->
            case t of
                Medium ->
                    True

                _ ->
                    False

        ( Just _, Just _ ) ->
            False

        ( Nothing, Just _ ) ->
            False

        ( Nothing, Nothing ) ->
            False


type MapTransition
    = Enter Int String
    | Leave Int String String


type alias Model =
    { maps : Dict String (HexEntityMap Tile Entity)
    , mapTransition : MapTransition
    , player : Player
    , selectedEntity : Maybe ( Point, Entity )
    , renderConfig : RenderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        ([ Content.Maps.testMap2
         , Content.Maps.testMap3
         ]
            |> Dict.fromList
        )
        (Enter transitionTime (Tuple.first Content.Maps.testMap2))
        (Player.new ( 0, 0, 0 ) '🐼')
        Nothing
        Render.initRenderConfig
    , Cmd.none
    )



-- MAP TRANSITION


transitionTime : Int
transitionTime =
    500


currentMap : Model -> HexEntityMap Tile Entity
currentMap model =
    (case model.mapTransition of
        Enter _ map ->
            Dict.get map model.maps

        Leave _ from _ ->
            Dict.get from model.maps
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


type Msg
    = FocusTile Point
    | Tick Int
    | MapTransition String
    | SelectEntity ( Point, Entity )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusTile point ->
            ( { model
                | player = Player.playerpath (isWalkable <| currentMap model) point model.player
              }
            , Cmd.none
            )

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
            ( { model | mapTransition = Leave 500 (currentMapName model) destination }, Cmd.none )

        SelectEntity entity ->
            ( { model | selectedEntity = Just entity }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , Render.renderTileEntityMap model.renderConfig
            (currentMap model)
            (View.viewTile FocusTile)
            (View.viewEntity MapTransition SelectEntity)
            [ View.viewPlayerMoveTarget model.player
            , View.viewPlayer model.player
            , View.viewEntityInteractions model.selectedEntity
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
