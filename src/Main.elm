module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Character exposing (Character)
import Content.Map
import HexEngine.Entity as Entity
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


type alias Model =
    { selectedPoint : Maybe Point
    , renderConfig : RenderConfig
    , world : World Tile Character
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
            ( playerPosition, '🐼' )
            [ ( ( 0, -3, 3 ), '🌺' )
            , ( ( 3, -2, -1 ), '🌻' )
            ]
            |> World.addMap
                ( 8, 5, -13 )
                Content.Map.testGrid2
                [ ( ( 1, 0, -1 ), '🌵' ) ]
            |> World.addMap
                ( -10, 5, 5 )
                Content.Map.testGrid3
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
isWalkable : World Tile Character -> Point -> Bool
isWalkable world point =
    case World.getPoint point world of
        ( Just Grass, Nothing ) ->
            True

        _ ->
            False


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


viewDebug : World Tile Character -> Html Msg
viewDebug world =
    let
        button : Point -> List ( Point, Tile ) -> Maybe (Html Msg)
        button mapPos _ =
            Just <|
                Html.button
                    [ Html.Events.onClick <| MapTransition mapPos ( 0, 0, 0 ) ]
                    [ Html.text <| "Travel to " ++ Point.toString mapPos ]
    in
    Html.div [ Html.Attributes.class "debug" ]
        (World.filterMapGrids button world)


renderTile : ( Point, Tile ) -> Svg Msg
renderTile =
    View.viewTile ClickHex


renderEntity : ( Point, Entity.Entity Character ) -> Svg Msg
renderEntity =
    View.viewEntity ClickEntity


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , viewDebug model.world
        , Render.viewWorld2
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
