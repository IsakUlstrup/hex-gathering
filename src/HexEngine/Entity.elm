module HexEngine.Entity exposing (Entity, WorldPosition, findPath, findPathAdjacent, getPosition, mapTransition, move, new, stateString, tickCooldown, worldPositionToString)

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

Moving: Cooldown, Path, Position

Cooling: Cooldown, Path, Position

BlockedPath: Cooldown, Position

Idle: Position

MapTransitionCharge: Cooldown, Position from, Position to

MapTransitionMove: Cooldown, Position from, Position to

-}
type EntityState
    = Moving Int (List Point) WorldPosition
    | Cooling Int (List Point) WorldPosition
    | BlockedPath Int WorldPosition
    | Idle WorldPosition
    | MapTransitionCharge Int WorldPosition WorldPosition
    | MapTransitionMove Int WorldPosition WorldPosition


{-| Entity wraps around provied entityData and adds position, state and a unique id
-}
type alias Entity entityData =
    { id : Int
    , state : EntityState
    , data : entityData
    }


new : Int -> Point -> Point -> entityData -> Entity entityData
new id mapPosition localPosition data =
    Entity id (Idle <| WorldPosition mapPosition localPosition) data


stateString : Entity entityData -> String
stateString entity =
    case entity.state of
        Moving _ _ _ ->
            "moving"

        Cooling _ _ _ ->
            "cooling"

        BlockedPath _ _ ->
            "blocked"

        Idle _ ->
            "idle"

        MapTransitionCharge _ _ _ ->
            "map-transition-charge"

        MapTransitionMove _ _ _ ->
            "map-transition-move"


worldPositionToString : WorldPosition -> String
worldPositionToString position =
    Point.toString position.map ++ "," ++ Point.toString position.local


getPosition : Entity entityData -> WorldPosition
getPosition entity =
    case entity.state of
        Moving _ _ position ->
            position

        Cooling _ _ position ->
            position

        BlockedPath _ position ->
            position

        Idle position ->
            position

        MapTransitionCharge _ from _ ->
            from

        MapTransitionMove _ _ to ->
            to


setPath : List Point -> Entity entityData -> Entity entityData
setPath path entity =
    case entity.state of
        Moving cd _ position ->
            { entity | state = Moving cd path position }

        Cooling cd _ position ->
            { entity | state = Cooling cd path position }

        BlockedPath _ position ->
            { entity | state = Cooling 200 path position }

        Idle position ->
            { entity | state = Cooling 200 path position }

        MapTransitionCharge _ _ _ ->
            entity

        MapTransitionMove _ _ _ ->
            entity


mapTransition : Int -> Point -> Point -> Entity entityData -> Entity entityData
mapTransition transitionDuration mapPosition localPosition entity =
    { entity | state = MapTransitionCharge transitionDuration (getPosition entity) (WorldPosition mapPosition localPosition) }


findPath : (Point -> Bool) -> Point -> Entity entityData -> Entity entityData
findPath walkable to entity =
    case Point.pathfind walkable (getPosition entity).local to of
        Just path ->
            setPath path entity

        Nothing ->
            { entity | state = BlockedPath 200 (getPosition entity) }


{-| find shortest path to a tile adjacent to target tile
-}
findPathAdjacent : (Point -> Bool) -> Point -> Entity entityData -> Entity entityData
findPathAdjacent walkable to entity =
    Point.neighbors to
        |> Set.toList
        |> List.filterMap (Point.pathfind walkable (getPosition entity).local)
        |> List.sortBy List.length
        |> List.head
        |> (\p ->
                case p of
                    Just path ->
                        setPath path entity

                    Nothing ->
                        { entity | state = BlockedPath 200 (getPosition entity) }
           )


tickCooldown : Int -> Entity entityData -> Entity entityData
tickCooldown dt entity =
    let
        cdDec a b =
            max 0 (a - b)
    in
    case entity.state of
        Moving cd path position ->
            { entity | state = Moving (cdDec cd dt) path position }

        Cooling cd path position ->
            { entity | state = Cooling (cdDec cd dt) path position }

        BlockedPath cd position ->
            { entity | state = BlockedPath (cdDec cd dt) position }

        Idle _ ->
            entity

        MapTransitionCharge cd from to ->
            { entity | state = MapTransitionCharge (cdDec cd dt) from to }

        MapTransitionMove cd from to ->
            { entity | state = MapTransitionMove (cdDec cd dt) from to }


move : Int -> Entity entityData -> Entity entityData
move moveTime entity =
    case entity.state of
        Moving cd path position ->
            if cd == 0 then
                { entity | state = Cooling 200 path position }

            else
                entity

        Cooling cd (p :: path) position ->
            if cd == 0 then
                { entity | state = Moving moveTime path (WorldPosition position.map p) }

            else
                entity

        Cooling _ [] position ->
            { entity | state = Idle position }

        BlockedPath cd position ->
            if cd <= 0 then
                { entity | state = Idle position }

            else
                entity

        Idle _ ->
            entity

        MapTransitionCharge 0 from to ->
            { entity | state = MapTransitionMove 500 from to }

        MapTransitionCharge _ _ _ ->
            entity

        MapTransitionMove 0 _ to ->
            { entity | state = Idle to }

        MapTransitionMove _ _ _ ->
            entity



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
