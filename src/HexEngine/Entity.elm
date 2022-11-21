module HexEngine.Entity exposing (Entity, WorldPosition, findPath, findPathAdjacent, move, new, setPosition, stateString, tickCooldown, worldPositionToString)

import HexEngine.Point as Point exposing (Point)
import Set



-- ENTITY


{-| World position, local represents position relative to current map, mapPosition represents map position
-}
type alias WorldPosition =
    { map : Point
    , local : Point
    }


{-| Entity state
-}
type EntityState
    = Moving (List Point) Int
    | Cooling (List Point)
    | BlockedPath Int
    | Idle


{-| Entity wraps around provied entityData and adds position, state and a unique id
-}
type alias Entity entityData =
    { id : Int
    , position : WorldPosition
    , state : EntityState
    , data : entityData
    }


new : Int -> Point -> Point -> entityData -> Entity entityData
new id mapPosition localPosition data =
    Entity id (WorldPosition mapPosition localPosition) Idle data


stateString : Entity entityData -> String
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


worldPositionToString : WorldPosition -> String
worldPositionToString position =
    Point.toString position.map ++ "," ++ Point.toString position.local


setPath : List Point -> Entity entityData -> Entity entityData
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


setPosition : WorldPosition -> Entity entityData -> Entity entityData
setPosition position entity =
    { entity | position = position }
        |> setPath [ position.local ]


findPath : (Point -> Bool) -> Point -> Entity entityData -> Entity entityData
findPath walkable to player =
    case Point.pathfind walkable player.position.local to of
        Just path ->
            setPath path player

        Nothing ->
            { player | state = BlockedPath 200 }


{-| find shortest path to a tile adjacent to target tile
-}
findPathAdjacent : (Point -> Bool) -> Point -> Entity entityData -> Entity entityData
findPathAdjacent walkable to player =
    Point.neighbors to
        |> Set.toList
        |> List.filterMap (Point.pathfind walkable player.position.local)
        |> List.sortBy List.length
        |> List.head
        |> (\p ->
                case p of
                    Just path ->
                        setPath path player

                    Nothing ->
                        { player | state = BlockedPath 200 }
           )


tickCooldown : Int -> Entity entityData -> Entity entityData
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


move : Int -> Entity entityData -> Entity entityData
move moveTime player =
    case player.state of
        Moving path cd ->
            if cd == 0 then
                { player | state = Cooling path }

            else
                player

        Cooling (p :: path) ->
            { player
                | state = Moving path moveTime
                , position = { map = player.position.map, local = p }
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



-- isIdle : Entity entityData -> Bool
-- isIdle player =
--     case player.state of
--         Idle ->
--             True
--         _ ->
--             False
-- hasPath : Entity entityData -> Bool
-- hasPath player =
--     case player.state of
--         Idle ->
--             False
--         BlockedPath _ ->
--             False
--         Moving _ _ ->
--             True
--         Cooling _ ->
--             True
-- readyToInteract : Entity entityData -> Point -> Bool
-- readyToInteract player point =
--     Point.distance point player.position.local == 1 && isIdle player
-- resetPosition : Entity entityData -> Entity entityData
-- resetPosition player =
--     { player | position = { mapOffset = player.position.mapOffset, local = ( 0, 0, 0 ) } }
