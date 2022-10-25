module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Maps
import Dict
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)
import HexEngine.Render as Render exposing (RenderConfig)
import Html exposing (Html, main_)
import Player exposing (Player)
import Tile exposing (Tile(..))
import View



-- TODO
-- Make EntityMap module that combines map and entities
-- MODEL


isWalkable : HexMap Tile -> Point -> Bool
isWalkable map point =
    case Dict.get point map of
        Just tile ->
            case tile of
                Medium ->
                    True

                _ ->
                    False

        Nothing ->
            False


type alias Model =
    { map : HexMap Tile
    , player : Player
    , tree : Player
    , renderConfig : RenderConfig
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Maps.testMap
        (Player.new ( 0, 0, 0 ) '🐼')
        (Player.new ( 6, -3, -3 ) '🌲')
        Render.initRenderConfig
    , Cmd.none
    )



-- UPDATE


type Msg
    = FocusTile Point
    | Tick Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusTile point ->
            ( { model
                | player = Player.playerpath (isWalkable model.map) point model.player
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | player =
                    model.player
                        |> Player.playerMove
                        |> Player.playerCooldown dt
                , renderConfig = Render.withHexFocus model.player.position model.renderConfig
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ Render.renderGrid model.renderConfig
            model.map
            (View.viewTile FocusTile)
            [ View.viewPlayerMoveTarget model.player
            , View.viewPlayer model.tree
            , View.viewPlayer model.player
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
