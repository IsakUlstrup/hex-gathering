module Player exposing
    ( Player
    , PlayerState
    , findPath
    , findPathAdjacent
    , hasPath
    , move
    , new
    , readyToInteract
    , stateString
    , stop
    , tickCooldown
    , travelTo
    )

import AnimationConstants
import HexEngine.Point as Point exposing (Point)
import Set


type PlayerState
    = Moving (List Point) Int
    | Cooling (List Point)
    | BlockedPath Int
    | MapEnter Int String
    | MapLeave Int String String
    | Idle


type alias Player =
    { position : Point
    , map : String
    , icon : Char
    , state : PlayerState
    }


new : String -> Point -> Char -> Player
new map position icon =
    Player position map icon (MapEnter (AnimationConstants.mapTransitionDuration |> Tuple.second) map)


stateString : Player -> String
stateString player =
    case player.state of
        Moving _ _ ->
            "moving"

        Cooling _ ->
            "cooling"

        BlockedPath _ ->
            "blocked"

        MapEnter _ _ ->
            "map-enter"

        MapLeave _ _ _ ->
            "map-leave"

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

        MapEnter _ _ ->
            player

        MapLeave _ _ _ ->
            player

        Idle ->
            { player | state = Cooling path }


findPath : (Point -> Bool) -> Point -> Player -> Player
findPath walkable to player =
    if moveTarget player == to then
        { player | state = Idle }

    else
        case Point.pathfind walkable player.position to of
            Just path ->
                setPath path player

            Nothing ->
                { player | state = BlockedPath 200 }


{-| find shortest path to a tile adjacent to target tile
-}
findPathAdjacent : (Point -> Bool) -> Point -> Player -> Player
findPathAdjacent walkable to player =
    if moveTarget player == to then
        { player | state = Idle }

    else
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

        MapEnter 0 _ ->
            { player | state = Idle }

        MapEnter cd mapName ->
            { player | state = MapEnter (max 0 (cd - dt)) mapName }

        MapLeave 0 _ to ->
            { player
                | state = MapEnter (AnimationConstants.mapTransitionDuration |> Tuple.second) to
                , map = to
            }

        MapLeave cd from to ->
            { player | state = MapLeave (max 0 (cd - dt)) from to }

        Idle ->
            player


stop : Player -> Player
stop player =
    { player | state = Idle }


moveTarget : Player -> Point
moveTarget player =
    (case player.state of
        Moving path _ ->
            path |> List.reverse |> List.head

        Cooling path ->
            path |> List.reverse |> List.head

        BlockedPath _ ->
            Nothing

        MapEnter _ _ ->
            Nothing

        MapLeave _ _ _ ->
            Nothing

        Idle ->
            Nothing
    )
        |> Maybe.withDefault player.position


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

        MapEnter _ _ ->
            player

        MapLeave _ _ _ ->
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

        MapEnter _ _ ->
            False

        MapLeave _ _ _ ->
            False


readyToInteract : Player -> Point -> Bool
readyToInteract player point =
    Point.distance point player.position == 1 && isIdle player


travelTo : String -> Player -> Player
travelTo mapName player =
    { player
        | state = MapLeave (AnimationConstants.mapTransitionDuration |> Tuple.second) player.map mapName
        , position = ( 0, 0, 0 )
    }
