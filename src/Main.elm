module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Entities
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


type alias Model =
    { maps : List (HexMap Tile)
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
        (Player.new Content.Maps.testMap.name ( 0, 0, 0 ) '🐼')
        Nothing
        (Render.initRenderConfig |> Render.withZoom 1.2)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Int
    | MapTransition String
    | ClickHex Point
    | CloseModal


getMap : String -> List (HexMap Tile) -> Maybe (HexMap Tile)
getMap name maps =
    maps |> List.filter (\m -> m.name == name) |> List.head


currentMap : Model -> HexMap Tile
currentMap model =
    getMap model.player.map model.maps
        |> Maybe.withDefault Content.Maps.errorMap


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
                | player = Player.travelTo destination model.player
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
                            Player.findPath (Tile.isWalkable <| currentMap model) point model.player

                        Just (TerrainEntity _ _) ->
                            Player.findPathAdjacent (Tile.isWalkable <| currentMap model) point model.player

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
            ( { model | selectedPoint = Nothing }
            , Cmd.none
            )



-- VIEW


viewEntityModal : Model -> Html Msg
viewEntityModal model =
    model.selectedPoint
        |> Maybe.map
            (\p ->
                case Tile.getEntity p (currentMap model) of
                    Just e ->
                        if Player.readyToInteract model.player p then
                            View.entityModal True MapTransition CloseModal e

                        else
                            View.entityModal False MapTransition CloseModal e

                    Nothing ->
                        View.entityModal False MapTransition CloseModal Content.Entities.awesomesaurus
            )
        |> Maybe.withDefault (View.entityModal False MapTransition CloseModal Content.Entities.awesomesaurus)


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , Render.renderMap model.renderConfig
            (currentMap model)
            (View.viewTile model.player.position model.selectedPoint ClickHex)
            [ View.viewPlayer model.player ]
        , viewEntityModal model
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
