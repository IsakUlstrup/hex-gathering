module Main exposing (Model, Msg, main)

import AnimationConstants
import Browser
import Browser.Events
import Content.Map
import Dict exposing (Dict)
import Entities.Counter
import Entities.Timer
import Entity exposing (Entity)
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Island exposing (Island)
import Player exposing (Player)
import Tile exposing (Tile(..))
import View



-- MODEL


type alias Model =
    { selectedIsland : ( Point, Island Tile Entity )
    , allIslands : Dict Point (Island Tile Entity)
    , player : Player
    , selectedPoint : Maybe Point
    , renderConfig : RenderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Map.testIsland
        (Dict.fromList [ Content.Map.testIsland2 ])
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
    | CounterMsg Point Entities.Counter.Msg
    | TimerMsg Point Entities.Timer.Msg



-- MAP


updateSelectedMapEntity : Point -> (Entity -> Entity) -> Model -> Model
updateSelectedMapEntity position f model =
    { model | selectedIsland = Tuple.mapSecond (Island.updateEntity position f) model.selectedIsland }



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


setSelected : Point -> Island Tile Entity -> Model -> Model
setSelected coordinate island model =
    { model
        | allIslands =
            model.allIslands
                |> Dict.insert (Tuple.first model.selectedIsland) (Tuple.second model.selectedIsland)
                |> Dict.remove coordinate
        , selectedIsland = ( coordinate, island )
    }


selectMap : Point -> Model -> Model
selectMap coordinate model =
    if Tuple.first model.selectedIsland == coordinate then
        model

    else
        case Dict.get coordinate model.allIslands of
            Just i ->
                setSelected coordinate i model

            Nothing ->
                model


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
                | player = Player.resetPosition model.player
                , selectedPoint = Nothing
              }
                |> selectMap destination
            , Cmd.none
            )

        ClickHex point ->
            let
                newPlayer : Player
                newPlayer =
                    case Island.getPoint point (Tuple.second model.selectedIsland) of
                        ( Just _, Just _ ) ->
                            Player.findPathAdjacent (Tile.isWalkable <| (Tuple.second model.selectedIsland).grid) point model.player

                        ( Nothing, Just _ ) ->
                            Player.findPath (Tile.isWalkable <| (Tuple.second model.selectedIsland).grid) point model.player

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

        CounterMsg position counterMsg ->
            ( model |> updateSelectedMapEntity position (Entity.counterUpdate counterMsg), Cmd.none )

        TimerMsg position timerMsg ->
            ( model |> updateSelectedMapEntity position (Entity.timerUpdate timerMsg), Cmd.none )



-- VIEW


viewEntityModal : Model -> Html Msg
viewEntityModal model =
    model.selectedPoint
        |> Maybe.map
            (\p ->
                case Dict.get p (Tuple.second model.selectedIsland).entities of
                    Just e ->
                        if Player.readyToInteract model.player p then
                            entityModal True MapTransition CloseModal p e

                        else
                            entityModal False MapTransition CloseModal p e

                    Nothing ->
                        entityModal False MapTransition CloseModal p (Entity.Counter Entities.Counter.init)
            )
        |> Maybe.withDefault (entityModal False MapTransition CloseModal ( 0, 0, 0 ) (Entity.Counter Entities.Counter.init))


entityModal : Bool -> (Point -> Msg) -> Msg -> Point -> Entity -> Html Msg
entityModal visible _ closeMsg position entity =
    let
        content : List (Html Msg)
        content =
            case entity of
                Entity.Counter model ->
                    [ Html.map (CounterMsg position) (Entities.Counter.view model) ]

                Entity.Timer model ->
                    [ Html.map (TimerMsg position) (Entities.Timer.view model) ]

                Entity.Player _ ->
                    [ Html.h1 [ Html.Attributes.class "entity-header" ] [ Html.text "Test" ]
                    , Html.p [] [ Html.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam posuere tincidunt nibh. Praesent enim dui, sagittis condimentum fermentum id, pulvinar eu quam. Nam aliquam tincidunt viverra. Vestibulum pulvinar est sit amet orci pellentesque, at gravida arcu vehicula. Suspendisse venenatis laoreet neque, vel tempus libero auctor eu. Nulla at scelerisque leo. Ut et turpis nulla. Ut cursus lorem sem, nec consequat orci pharetra id. " ]
                    ]
    in
    Html.aside
        [ Html.Attributes.class "modal-container"
        , Html.Attributes.classList [ ( "visible", visible ) ]
        ]
        [ Html.div [ Html.Attributes.class "modal-content" ]
            (Html.button [ Html.Events.onClick closeMsg ] [ Html.text "x" ] :: content)
        ]


view : Model -> Html Msg
view model =
    main_ []
        [ AnimationConstants.styleNode [ AnimationConstants.fallDuration, AnimationConstants.playerMoveTime ]
        , Render.entityMap model.renderConfig
            (Tuple.second model.selectedIsland).grid
            (View.viewTile model.player.position model.selectedPoint ClickHex)
            (( model.player.position, Entity.Player model.player ) :: ((Tuple.second model.selectedIsland).entities |> Dict.toList))
            View.viewEntity
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
