module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Map
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Island exposing (IslandMap)
import Player exposing (Player)
import Tile exposing (Tile(..))
import View



-- MODEL


type alias Model =
    { maps : IslandMap Tile
    , player : Player
    , selectedPoint : Maybe Point
    , renderConfig : RenderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Map.testMap
        (Player.new ( 0, 0, 0 ) '🐼')
        Nothing
        (Render.initRenderConfig |> Render.withZoom 1.2)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Int
    | MapTransition Point
    | ClickHex Point
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | player =
                    model.player
                        |> Player.tickCooldown dt
                        |> Player.move
                , renderConfig =
                    model.selectedPoint
                        |> Maybe.map
                            (\p ->
                                if Player.readyToInteract model.player p then
                                    Render.withHexFocus p model.renderConfig

                                else
                                    Render.withHexFocus model.player.position model.renderConfig
                            )
                        |> Maybe.withDefault (Render.withHexFocus model.player.position model.renderConfig)
              }
            , Cmd.none
            )

        MapTransition destination ->
            ( { model
                | maps = Island.selectMap destination model.maps
                , player = Player.resetPosition model.player
                , selectedPoint = Nothing
              }
            , Cmd.none
            )

        ClickHex point ->
            let
                newPlayer : Player
                newPlayer =
                    Player.findPath (Tile.isWalkable <| (Tuple.second model.maps.selected).grid) point model.player

                -- case Dict.get point (Tuple.second model.maps.selected).grid of
                --     Just (Terrain _) ->
                --         Player.findPath (Tile.isWalkable <| (Tuple.second model.maps.selected).grid) point model.player
                --     Just (TerrainEntity _ _) ->
                --         Player.findPathAdjacent (Tile.isWalkable <| (Tuple.second model.maps.selected).grid) point model.player
                --     _ ->
                --         model.player
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
            ( { model | selectedPoint = Nothing }
            , Cmd.none
            )



-- VIEW
-- viewEntityModal : Model -> Html Msg
-- viewEntityModal model =
--     model.selectedPoint
--         |> Maybe.map
--             (\p ->
--                 case Tile.getEntity p (Tuple.second model.maps.selected).grid of
--                     Just e ->
--                         if Player.readyToInteract model.player p then
--                             View.entityModal True MapTransition CloseModal e
--                         else
--                             View.entityModal False MapTransition CloseModal e
--                     Nothing ->
--                         View.entityModal False MapTransition CloseModal Content.Entities.awesomesaurus
--             )
--         |> Maybe.withDefault (View.entityModal False MapTransition CloseModal Content.Entities.awesomesaurus)


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , Render.renderMap model.renderConfig
            (Tuple.second model.maps.selected).grid
            (View.viewTile model.player.position model.selectedPoint ClickHex)
            [ View.viewPlayer model.player ]

        -- , viewEntityModal model
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
