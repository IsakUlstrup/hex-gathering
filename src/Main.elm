module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Map
import Entity exposing (Entity)
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import HexEngine.World as World exposing (World)
import Html exposing (Html, main_)
import Tile exposing (Tile(..))
import View



-- MODEL


type alias Model =
    { --      selectedIsland : EntityMap Tile Entity
      -- , allIslands : Dict Point (EntityMap Tile Entity)
      -- , player : Player
      selectedPoint : Maybe Point
    , renderConfig : RenderConfig
    , world : World Tile Entity
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        -- Content.Map.testIsland
        -- (Dict.fromList [ ( HexEngine.EntityMap.getOffset Content.Map.testIsland2, Content.Map.testIsland2 ) ])
        -- (Player.new ( 0, 0, 0 ) '🐼')
        Nothing
        (Render.initRenderConfig |> Render.withZoom 1.2)
        (World.newWorld ( 2, -1, -1 ) (World.newMap "Test" Content.Map.testGrid) '🐼'
            |> World.addEntity ( 2, -1, -1 ) ( 0, -3, 3 ) '🌺'
            |> World.addEntity ( 2, -1, -1 ) ( 3, -2, -1 ) '🌺'
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
      -- | MapTransition Point
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
                            (World.tickCooldown dt
                                >> World.move (Tuple.second AnimationConstants.playerMoveTime)
                            )
                , renderConfig = Render.withEntityFocus (World.getPlayer model.world).position model.renderConfig
              }
            , Cmd.none
            )

        -- MapTransition _ ->
        --     -- ( { model
        --     --     | player = Player.resetPosition model.player
        --     --     , selectedPoint = Nothing
        --     --   }
        --     --     |> selectMap destination
        --     -- , Cmd.none
        --     -- )
        --     ( model, Cmd.none )
        ClickHex point ->
            ( { model
                | world =
                    World.updatePlayer (World.findPath (isWalkable model.world) point) model.world
                , selectedPoint =
                    Just point
              }
            , Cmd.none
            )

        ClickEntity point ->
            ( { model
                | world =
                    World.updatePlayer (World.findPathAdjacent (isWalkable model.world) point) model.world
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


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]

        -- , Render.entityMap model.renderConfig
        --     (model.selectedIsland |> HexEngine.EntityMap.addEntity model.player.position model.player.icon)
        --     (View.viewTile model.player.position model.selectedPoint ClickHex)
        --     View.viewEntity2
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
