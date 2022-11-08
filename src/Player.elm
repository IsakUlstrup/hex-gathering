module Player exposing
    ( MoveState
    , Player
    , hasPath
    , moveStateString
    , new
    , playerCooldown
    , playerMove
    , playerpath
    , playerpathAdjacent
    , readyToInteract
    , stop
    )

import AnimationConstants
import HexEngine.Point as Point exposing (Point)
import Set


type MoveState
    = Moving (List Point) Int
    | Cooling (List Point)
    | BlockedPath Point Int
    | Idle


type alias Player =
    { position : Point
    , icon : Char
    , moveState : MoveState
    }


new : Point -> Char -> Player
new position icon =
    Player position icon Idle


moveStateString : Player -> String
moveStateString player =
    case player.moveState of
        Moving _ _ ->
            "moving"

        Cooling _ ->
            "cooling"

        BlockedPath _ _ ->
            "blocked"

        Idle ->
            "idle"


setPlayerPath : List Point -> Player -> Player
setPlayerPath path player =
    case player.moveState of
        Moving _ cd ->
            { player | moveState = Moving path cd }

        Cooling _ ->
            { player | moveState = Cooling path }

        BlockedPath _ _ ->
            { player | moveState = Cooling path }

        Idle ->
            { player | moveState = Cooling path }


playerpath : (Point -> Bool) -> Point -> Player -> Player
playerpath walkable to player =
    if moveTarget player == to then
        { player | moveState = Idle }

    else
        case Point.pathfind walkable player.position to of
            Just path ->
                setPlayerPath path player

            Nothing ->
                { player | moveState = BlockedPath to 200 }


{-| find shortest path to a tile adjacent to target tile
-}
playerpathAdjacent : (Point -> Bool) -> Point -> Player -> Player
playerpathAdjacent walkable to player =
    if moveTarget player == to then
        { player | moveState = Idle }

    else
        Point.neighbors to
            |> Set.toList
            |> List.filterMap (Point.pathfind walkable player.position)
            |> List.sortBy List.length
            |> List.head
            |> (\p ->
                    case p of
                        Just path ->
                            setPlayerPath path player

                        Nothing ->
                            { player | moveState = BlockedPath to 200 }
               )


playerCooldown : Int -> Player -> Player
playerCooldown dt player =
    case player.moveState of
        Moving path cd ->
            { player | moveState = Moving path (max 0 (cd - dt)) }

        Cooling path ->
            { player | moveState = Cooling path }

        BlockedPath to cd ->
            { player | moveState = BlockedPath to (max 0 (cd - dt)) }

        Idle ->
            player


stop : Player -> Player
stop player =
    { player | moveState = Idle }


moveTarget : Player -> Point
moveTarget player =
    (case player.moveState of
        Moving path _ ->
            path |> List.reverse |> List.head

        Cooling path ->
            path |> List.reverse |> List.head

        BlockedPath _ _ ->
            Nothing

        Idle ->
            Nothing
    )
        |> Maybe.withDefault player.position


playerMove : Player -> Player
playerMove player =
    case player.moveState of
        Moving path cd ->
            if cd == 0 then
                { player | moveState = Cooling path }

            else
                player

        Cooling (p :: path) ->
            { player
                | moveState = Moving path (Tuple.second AnimationConstants.playerMoveTime)
                , position = p
            }

        Cooling [] ->
            { player
                | moveState = Idle
            }

        BlockedPath _ cd ->
            if cd <= 0 then
                { player
                    | moveState = Idle
                }

            else
                player

        Idle ->
            player


isIdle : Player -> Bool
isIdle player =
    case player.moveState of
        Idle ->
            True

        _ ->
            False


hasPath : Player -> Bool
hasPath player =
    case player.moveState of
        Idle ->
            False

        BlockedPath _ _ ->
            False

        Moving _ _ ->
            True

        Cooling _ ->
            True


readyToInteract : Player -> Point -> Bool
readyToInteract player point =
    Point.distance point player.position == 1 && isIdle player
