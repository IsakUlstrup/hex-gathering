module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Map
import Entity exposing (Entity)
import HexEngine.Entity as Entity
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import HexEngine.World as World exposing (Map, World)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Tile exposing (Tile(..))
import View



-- MODEL


type alias Model =
    { selectedPoint : Maybe Point
    , renderConfig : RenderConfig
    , world : World Tile Entity
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( mapPosition, playerPosition ) =
            ( ( 2, -1, -1 ), ( 0, 0, 0 ) )
    in
    ( Model
        Nothing
        (Render.initRenderConfig
            |> Render.withEntityFocus (Entity.WorldPosition mapPosition playerPosition)
            |> Render.withZoom 1.2
        )
        (World.newWorld
            mapPosition
            (World.newMap "Test" Content.Map.testGrid)
            ( playerPosition, '🐼' )
            [ ( ( 0, -3, 3 ), '🌺' )
            , ( ( 3, -2, -1 ), '🌻' )
            ]
            |> World.addMap
                ( 5, 5, -10 )
                (World.newMap "Test2" Content.Map.testGrid2)
                [ ( ( 1, 0, -1 ), '🌵' ) ]
            |> World.addMap
                ( -10, 5, 5 )
                (World.newMap "Test3" Content.Map.testGrid3)
                []
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Int
    | MapTransition Point Point
    | ClickHex Point
    | ClickEntity Point


{-| determine if a given point is walkable

Only tiles that exist and are of variant Medium are walkable

Entities are not walkable

-}
isWalkable : World Tile Entity -> Point -> Bool
isWalkable world point =
    case World.getPoint point world of
        ( Just Medium, Nothing ) ->
            True

        _ ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | world =
                    World.movementUpdate dt AnimationConstants.playerMoveTime.value model.world
                , renderConfig =
                    Render.withPlayerFocus model.world model.renderConfig
              }
            , Cmd.none
            )

        MapTransition map position ->
            ( { model
                | world = World.playerMoveMap 200 map position model.world
                , selectedPoint = Nothing
              }
            , Cmd.none
            )

        ClickHex point ->
            ( { model
                | world =
                    World.updatePlayer (Entity.findPath (isWalkable model.world) point) model.world
                , selectedPoint =
                    Just point
              }
            , Cmd.none
            )

        ClickEntity point ->
            ( { model
                | world =
                    World.updatePlayer (Entity.findPathAdjacent (isWalkable model.world) point) model.world
                , selectedPoint =
                    Just point
              }
            , Cmd.none
            )



-- VIEW


viewDebug : World Tile Entity -> Html Msg
viewDebug world =
    let
        button : ( Point, Map Tile ) -> Html Msg
        button ( coordinate, _ ) =
            Html.button
                [ Html.Events.onClick <| MapTransition coordinate ( 0, 0, 0 ) ]
                [ Html.text <| "Travel to " ++ Point.toString coordinate ]
    in
    Html.div [ Html.Attributes.class "debug" ]
        (World.mapMaps button world)


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , viewDebug model.world
        , Render.viewWorld2
            model.renderConfig
            model.world
            (View.viewTile (World.getPlayerPosition model.world).local model.selectedPoint ClickHex)
            (View.viewEntity ClickEntity)
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
