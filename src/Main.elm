module Main exposing (Model, Msg, Selected, main)

import AnimationConstants
import Browser
import Browser.Events
import Character exposing (Character, CharacterInteraction(..))
import Content.Characters
import Content.Map
import HexEngine.Entity as Entity exposing (WorldPosition)
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import HexEngine.World as World exposing (World)
import Html exposing (Html, main_)
import Svg exposing (Svg)
import Tile exposing (Tile(..))
import View



-- MODEL


type Selected
    = Entity Point
    | Tile


type alias Model =
    { selectedPoint : Maybe Point
    , renderConfig : RenderConfig
    , world : World Tile Character
    , selected : Maybe Selected
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
            |> Render.withZoom 0.2
        )
        (World.newWorld
            mapPosition
            Content.Map.testGrid
            ( playerPosition, Content.Characters.panda )
            [ ( ( 0, -3, 3 ), Content.Characters.hibiscus )
            , ( ( -1, 3, -2 ), Content.Characters.counter )
            , ( ( 3, -2, -1 )
              , Content.Characters.busStop
                    [ WorldPosition ( 8, 5, -13 ) ( 0, 0, 0 )
                    , WorldPosition ( -10, 5, 5 ) ( 0, 0, 0 )
                    , WorldPosition ( -10, 5, 5 ) ( 0, 0, 0 )
                    , WorldPosition ( -10, 5, 5 ) ( 0, 0, 0 )
                    , WorldPosition ( -10, 5, 5 ) ( 0, 0, 0 )
                    , WorldPosition ( -10, 5, 5 ) ( 0, 0, 0 )
                    ]
              )
            ]
            |> World.addMap
                ( 8, 5, -13 )
                Content.Map.testGrid2
                [ ( ( 1, 0, -1 ), Content.Characters.sunflower )
                , ( ( -1, 2, -1 ), Content.Characters.busStop [ WorldPosition mapPosition ( 0, 0, 0 ) ] )
                ]
            |> World.addMap
                ( -10, 5, 5 )
                Content.Map.testGrid3
                [ ( ( -1, 2, -1 ), Content.Characters.busStop [ WorldPosition mapPosition ( 0, 0, 0 ) ] )
                , ( ( 1, 1, -2 ), Content.Characters.cactus )
                ]
        )
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Int
    | ClickHex Point
    | ClickEntity Point
    | CharacterAction CharacterInteraction


{-| determine if a given point is walkable

Only tiles that exist and are of variant Medium are walkable

Entities are not walkable

-}
isWalkable : World Tile Character -> Point -> Bool
isWalkable world point =
    case World.getPoint point world of
        ( Just Grass, Nothing ) ->
            True

        _ ->
            False


zoomIfAdjacent : Maybe Selected -> World Tile Character -> RenderConfig -> RenderConfig
zoomIfAdjacent selected world config =
    if selectedEntityAdjacent selected world then
        Render.withZoom 0.35 config

    else
        Render.withZoom 0.2 config


cameraFocus : Maybe Selected -> World Tile Character -> RenderConfig -> RenderConfig
cameraFocus selected world config =
    case selected of
        Just (Entity p) ->
            if selectedEntityAdjacent selected world then
                Render.withEntityFocus (WorldPosition (World.getPlayerPosition world).map p) config

            else
                Render.withPlayerFocus world config

        _ ->
            Render.withPlayerFocus world config



-- if selectedEntityAdjacent selected world then
--     Render.withEntityFocus
-- else
--     Render.withZoom 0.3 config


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | world =
                    model.world
                        |> World.updateEntities (Entity.tickCooldown dt)
                        |> World.updateEntities (Entity.move AnimationConstants.playerMoveTime.value)
                , renderConfig =
                    model.renderConfig
                        |> cameraFocus model.selected model.world
                        |> zoomIfAdjacent model.selected model.world
              }
            , Cmd.none
            )

        ClickHex point ->
            ( { model
                | world =
                    World.updatePlayer (Entity.findPath (isWalkable model.world) point) model.world
                , selected =
                    Just Tile
              }
            , Cmd.none
            )

        ClickEntity point ->
            ( { model
                | world =
                    World.updatePlayer (Entity.findPathAdjacent (isWalkable model.world) point) model.world
                , selected =
                    case model.selected of
                        Just (Entity p) ->
                            if point == p then
                                Nothing

                            else
                                Just <| Entity point

                        _ ->
                            Just <| Entity point
              }
            , Cmd.none
            )

        CharacterAction action ->
            case action of
                Travel coords ->
                    ( { model
                        | world = World.playerMoveMap 200 coords.map coords.local model.world
                        , selectedPoint = Nothing
                      }
                    , Cmd.none
                    )

                IncrementCounter ->
                    ( model, Cmd.none )

                DecrementCounter ->
                    ( model, Cmd.none )



-- VIEW


selectedEntityAdjacent : Maybe Selected -> World Tile Character -> Bool
selectedEntityAdjacent selected world =
    case selected of
        Just (Entity p) ->
            Point.distance p (World.getPlayer world |> Entity.getPosition).local == 1

        _ ->
            False


selectedEntity : Maybe Selected -> World Tile Character -> Maybe Point
selectedEntity selected world =
    case selected of
        Just (Entity p) ->
            if Point.distance p (World.getPlayer world |> Entity.getPosition).local == 1 then
                Just p

            else
                Nothing

        _ ->
            Nothing


renderTile : ( Point, Tile ) -> Svg Msg
renderTile =
    View.viewTile ClickHex


renderEntity : Maybe Point -> ( Point, Entity.Entity Character ) -> Svg Msg
renderEntity selectedPoint =
    View.viewEntity selectedPoint CharacterAction ClickEntity


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]

        -- , viewDebug model.selected model.world
        , Render.viewWorld
            model.renderConfig
            View.svgDefs
            model.world
            renderTile
            (renderEntity (selectedEntity model.selected model.world))
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
