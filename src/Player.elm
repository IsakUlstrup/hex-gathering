module Player exposing
    ( Player
    , PlayerState
    , findPath
    , findPathAdjacent
    , hasPath
    , move
    , new
    , readyToInteract
    , resetPosition
    , stateString
    , tickCooldown
    )

import AnimationConstants
import HexEngine.Point as Point exposing (Point)
import Set


type PlayerState
    = Moving (List Point) Int
    | Cooling (List Point)
    | BlockedPath Int
    | Idle


type alias Player =
    { position : Point
    , icon : Char
    , state : PlayerState
    }


new : Point -> Char -> Player
new position icon =
    Player position icon Idle


stateString : Player -> String
stateString player =
    case player.state of
        Moving _ _ ->
            "moving"

        Cooling _ ->
            "cooling"

        BlockedPath _ ->
            "blocked"

        Idle ->
            "idle"


setPath : List Point -> Player -> Player
setPath path player =
    case player.state of
        Moving _ cd ->
            { player | state = Moving path cd }

        Cooling _ ->
            { player | state = Cooling path }

        BlockedPath _ ->
            { player | state = Cooling path }

        Idle ->
            { player | state = Cooling path }


findPath : (Point -> Bool) -> Point -> Player -> Player
findPath walkable to player =
    case Point.pathfind walkable player.position to of
        Just path ->
            setPath path player

        Nothing ->
            { player | state = BlockedPath 200 }


{-| find shortest path to a tile adjacent to target tile
-}
findPathAdjacent : (Point -> Bool) -> Point -> Player -> Player
findPathAdjacent walkable to player =
    Point.neighbors to
        |> Set.toList
        |> List.filterMap (Point.pathfind walkable player.position)
        |> List.sortBy List.length
        |> List.head
        |> (\p ->
                case p of
                    Just path ->
                        setPath path player

                    Nothing ->
                        { player | state = BlockedPath 200 }
           )


tickCooldown : Int -> Player -> Player
tickCooldown dt player =
    case player.state of
        Moving path cd ->
            { player | state = Moving path (max 0 (cd - dt)) }

        Cooling path ->
            { player | state = Cooling path }

        BlockedPath cd ->
            { player | state = BlockedPath (max 0 (cd - dt)) }

        Idle ->
            player


move : Player -> Player
move player =
    case player.state of
        Moving path cd ->
            if cd == 0 then
                { player | state = Cooling path }

            else
                player

        Cooling (p :: path) ->
            { player
                | state = Moving path (Tuple.second AnimationConstants.playerMoveTime)
                , position = p
            }

        Cooling [] ->
            { player
                | state = Idle
            }

        BlockedPath cd ->
            if cd <= 0 then
                { player
                    | state = Idle
                }

            else
                player

        Idle ->
            player


isIdle : Player -> Bool
isIdle player =
    case player.state of
        Idle ->
            True

        _ ->
            False


hasPath : Player -> Bool
hasPath player =
    case player.state of
        Idle ->
            False

        BlockedPath _ ->
            False

        Moving _ _ ->
            True

        Cooling _ ->
            True


readyToInteract : Player -> Point -> Bool
readyToInteract player point =
    Point.distance point player.position == 1 && isIdle player


resetPosition : Player -> Player
resetPosition player =
    { player | position = ( 0, 0, 0 ) }
