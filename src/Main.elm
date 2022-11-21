module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Map
import Entity exposing (Entity)
import HexEngine.Entity as Entity
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import HexEngine.World as World exposing (World)
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
    ( Model
        Nothing
        (Render.initRenderConfig
            |> Render.withEntityFocus (Entity.WorldPosition ( 2, -1, -1 ) ( 0, 0, 0 ))
            |> Render.withZoom 1.2
        )
        (World.newWorld
            ( 2, -1, -1 )
            (World.newMap "Test" Content.Map.testGrid)
            '🐼'
            |> World.addEntity ( 2, -1, -1 ) ( 0, -3, 3 ) '🌺'
            |> World.addEntity ( 2, -1, -1 ) ( 3, -2, -1 ) '🌺'
            |> World.addMap ( 5, 5, -10 ) (World.newMap "Test2" Content.Map.testGrid2)
        )
    , Cmd.none
    )



-- UPDATE
-- MAP
-- updateSelectedMapEntity : Point -> (Entity -> Entity) -> Model -> Model
-- updateSelectedMapEntity position f model =
--     { model | selectedIsland = Tuple.mapSecond (HexEngine.EntityMap.updateEntity position f) model.selectedIsland }
-- updateEntities : (Entity -> Entity) -> Model -> Model
-- updateEntities f model =
--     { model
--         | selectedIsland =
--             model.selectedIsland
--                 |> Tuple.mapSecond (Island.mapEntities f)
--         , allIslands =
--             model.allIslands
--                 |> Dict.map (\_ v -> Island.mapEntities f v)
--     }
-- setSelected : Point -> EntityMap Tile Entity -> Model -> Model
-- setSelected coordinate island model =
--     { model
--         | allIslands =
--             model.allIslands
--                 |> Dict.insert (HexEngine.EntityMap.getOffset model.selectedIsland) model.selectedIsland
--                 |> Dict.remove coordinate
--         , selectedIsland = island
--     }
-- selectMap : Point -> Model -> Model
-- selectMap coordinate model =
--     if HexEngine.EntityMap.getOffset model.selectedIsland == coordinate then
--         model
--     else
--         case Dict.get coordinate model.allIslands of
--             Just i ->
--                 setSelected coordinate i model
--             Nothing ->
--                 model


{-| determine if a given point is walkable

Only tiles that exist and are of variant Medium are walkable

Entities are not walkable

-}
isWalkable : World Tile Entity -> Point -> Bool
isWalkable island point =
    case World.getPoint point island of
        ( Just Medium, Nothing ) ->
            True

        _ ->
            False


type Msg
    = Tick Int
    | MapTransition Point Point
    | ClickHex Point
    | ClickEntity Point



-- | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            -- ( { model
            --     | player =
            --         model.player
            --             |> Player.tickCooldown dt
            --             |> Player.move
            --     , renderConfig =
            --         model.selectedPoint
            --             |> Maybe.map
            --                 (\p ->
            --                     if Player.readyToInteract model.player p then
            --                         Render.withHexFocus p model.renderConfig
            --                     else
            --                         Render.withHexFocus (World.getPlayer model.world).position.local model.renderConfig
            --                 )
            --             |> Maybe.withDefault (Render.withHexFocus (World.getPlayer model.world).position.local model.renderConfig)
            --   }
            -- , Cmd.none
            -- )
            ( { model
                | world =
                    model.world
                        |> World.updateEntities
                            (Entity.tickCooldown dt
                                >> Entity.move (Tuple.second AnimationConstants.playerMoveTime)
                            )
                , renderConfig = Render.withEntityFocus (World.getPlayer model.world).position model.renderConfig
              }
            , Cmd.none
            )

        MapTransition map position ->
            -- ( { model
            --     | player = Player.resetPosition model.player
            --     , selectedPoint = Nothing
            --   }
            --     |> selectMap destination
            -- , Cmd.none
            -- )
            ( { model
                | world = World.playerMoveMap map position model.world
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



-- ( { model | selectedPoint = Just point }, Cmd.none )
-- CloseModal ->
--     ( { model | selectedPoint = Nothing }
--     , Cmd.none
--     )
-- VIEW
-- viewEntityModal : Model -> Html Msg
-- viewEntityModal model =
--     model.selectedPoint
--         |> Maybe.map
--             (\p ->
--                 case Dict.get p (Tuple.second model.selectedIsland).entities of
--                     Just e ->
--                         if Player.readyToInteract model.player p then
--                             entityModal True MapTransition CloseModal p e
--                         else
--                             entityModal False MapTransition CloseModal p e
--                     Nothing ->
--                         entityModal False MapTransition CloseModal p ' '
--             )
--         |> Maybe.withDefault (entityModal False MapTransition CloseModal ( 0, 0, 0 ) ' ')
-- entityModal : Bool -> (Point -> Msg) -> Msg -> Point -> Entity -> Html Msg
-- entityModal visible _ closeMsg _ entity =
--     let
--         content : List (Html Msg)
--         content =
--             -- case entity of
--             -- Entity.Counter model ->
--             --     [ Html.map (CounterMsg position) (Entities.Counter.view model) ]
--             -- Entity.Timer model ->
--             --     [ Html.map (TimerMsg position) (Entities.Timer.view model) ]
--             -- Entity.Player _ ->
--             --     [ Html.h1 [ Html.Attributes.class "entity-header" ] [ Html.text "Test" ]
--             --     , Html.p [] [ Html.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam posuere tincidunt nibh. Praesent enim dui, sagittis condimentum fermentum id, pulvinar eu quam. Nam aliquam tincidunt viverra. Vestibulum pulvinar est sit amet orci pellentesque, at gravida arcu vehicula. Suspendisse venenatis laoreet neque, vel tempus libero auctor eu. Nulla at scelerisque leo. Ut et turpis nulla. Ut cursus lorem sem, nec consequat orci pharetra id. " ]
--             --     ]
--             [ Html.p [] [ Html.text <| String.fromChar entity ] ]
--     in
--     Html.aside
--         [ Html.Attributes.class "modal-container"
--         , Html.Attributes.classList [ ( "visible", visible ) ]
--         ]
--         [ Html.div [ Html.Attributes.class "modal-content" ]
--             (Html.button [ Html.Events.onClick closeMsg ] [ Html.text "x" ] :: content)
--         ]


viewDebug : Html Msg
viewDebug =
    Html.div [ Html.Attributes.class "debug" ]
        [ Html.button [ Html.Events.onClick <| MapTransition ( 2, -1, -1 ) ( 0, 0, 0 ) ] [ Html.text "Travel to ( 2, -1, -1 )" ]
        , Html.button [ Html.Events.onClick <| MapTransition ( 5, 5, -10 ) ( 0, 0, 0 ) ] [ Html.text "Travel to ( 5, 5, -10 )" ]
        ]


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , viewDebug
        , Render.viewWorld
            model.renderConfig
            model.world
            (View.viewTile (World.getPlayer model.world).position.local model.selectedPoint ClickHex)
            (View.viewEntity ClickEntity)

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
