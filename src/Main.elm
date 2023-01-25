module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Character exposing (Character, CharacterMsg(..))
import Content.Characters
import Content.Map
import HexEngine.Entity as Entity exposing (WorldPosition)
import HexEngine.Point as Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import HexEngine.World as World exposing (World)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Svg exposing (Svg)
import Tile exposing (Tile(..))
import View



-- MODEL


type Selected
    = Entity Point
    | Tile Point


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
            |> Render.withZoom 0.3
        )
        (World.newWorld
            mapPosition
            Content.Map.testGrid
            ( playerPosition, Content.Characters.panda )
            [ ( ( 0, -3, 3 ), Content.Characters.hibiscus )
            , ( ( 3, -2, -1 ), Content.Characters.airplane (WorldPosition ( 8, 5, -13 ) ( 0, 0, 0 )) )
            ]
            |> World.addMap
                ( 8, 5, -13 )
                Content.Map.testGrid2
                [ ( ( 1, 0, -1 ), Content.Characters.sunflower ) ]
            |> World.addMap
                ( -10, 5, 5 )
                Content.Map.testGrid3
                []
        )
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Int
    | MapTransition Point Point
    | ClickHex Point
    | ClickEntity Point
    | CharacterAction CharacterMsg


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
        Render.withZoom 0.4 config

    else
        Render.withZoom 0.3 config


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
                , selected =
                    Just <| Tile point
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



-- VIEW


selectedEntityAdjacent : Maybe Selected -> World Tile Character -> Bool
selectedEntityAdjacent selected world =
    case selected of
        Just (Entity p) ->
            Point.distance p (World.getPlayer world |> Entity.getPosition).local == 1

        _ ->
            False


viewDebug : Maybe Selected -> World Tile Character -> Html Msg
viewDebug selected world =
    let
        button : Point -> List ( Point, Tile ) -> Maybe (Html Msg)
        button mapPos _ =
            Just <|
                Html.button
                    [ Html.Events.onClick <| MapTransition mapPos ( 0, 0, 0 ) ]
                    [ Html.text <| "Travel to " ++ Point.toString mapPos ]

        adjacent =
            if selectedEntityAdjacent selected world then
                Html.p [] [ Html.text "adjacent" ]

            else
                Html.p [] [ Html.text "not entity adjacent" ]
    in
    Html.div [ Html.Attributes.class "debug" ]
        (adjacent :: World.filterMapGrids button world)


renderTile : ( Point, Tile ) -> Svg Msg
renderTile =
    View.viewTile ClickHex


renderEntity : ( Point, Entity.Entity Character ) -> Svg Msg
renderEntity =
    View.viewEntity CharacterAction ClickEntity


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , viewDebug model.selected model.world
        , Render.viewWorld
            model.renderConfig
            View.svgDefs
            model.world
            renderTile
            renderEntity
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
