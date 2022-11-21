module HexEngine.World exposing (Entity, EntityPosition, EntityState, Map, World, addEntity, findPath, findPathAdjacent, getPlayer, getPoint, mapCurrentEntities, mapCurrentGrid, move, newMap, newWorld, stateString, tickCooldown, updateEntities, updatePlayer)

import Dict exposing (Dict)
import HexEngine.HexGrid as Grid exposing (HexGrid)
import HexEngine.Point as Point exposing (Point)
import Set



-- WORLD


{-| A world represents an entire gameState. Includes multiple maps and entities along side a player

Handles entity Ids automatically

-}
type World tileData entityData
    = World
        { entities : List (Entity entityData)
        , maps : Dict Point (Map tileData)
        , player : Entity entityData
        , idCounter : Int
        }


{-| Create a new world with provided map and player. Both will be placed at (0, 0, 0)
-}
newWorld : Map tileData -> entityData -> World tileData entityData
newWorld initMap playerData =
    World
        { entities = []
        , maps = Dict.fromList [ ( ( 0, 0, 0 ), initMap ) ]
        , player = Entity 0 (EntityPosition ( 0, 0, 0 ) ( 0, 0, 0 )) Idle playerData
        , idCounter = 1
        }


{-| Add entity to world with generated id

TODO: make sure target map exists

-}
addEntity : Point -> Point -> entityData -> World tileData entityData -> World tileData entityData
addEntity mapOffset position entity (World world) =
    World
        { world
            | entities = Entity world.idCounter (EntityPosition position mapOffset) Idle entity :: world.entities
            , idCounter = world.idCounter + 1
        }


{-| Get map where player is located
-}
playerMap : World tileData entityData -> Maybe (Map tileData)
playerMap (World world) =
    Dict.get world.player.position.mapOffset world.maps


getPoint : Point -> World tileData entityData -> ( Maybe tileData, Maybe (Entity entityData) )
getPoint target (World world) =
    ( playerMap (World world)
        |> Maybe.andThen (\m -> Grid.get target m.grid)
    , world.entities
        |> List.filter (\e -> e.position.local == target)
        |> List.head
    )


updatePlayer : (Entity entityData -> Entity entityData) -> World tileData entityData -> World tileData entityData
updatePlayer f (World world) =
    World { world | player = f world.player }


updateEntities : (Entity entityData -> Entity entityData) -> World tileData entityData -> World tileData entityData
updateEntities f (World world) =
    World
        { world
            | player = f world.player
            , entities = List.map f world.entities
        }


{-| Map the grid player is located in
-}
mapCurrentGrid : (List ( Point, tileData ) -> a) -> World tileData entityData -> a
mapCurrentGrid f (World world) =
    case Dict.get world.player.position.mapOffset world.maps of
        Just map ->
            f (Grid.toList map.grid)

        Nothing ->
            f []


{-| Map Entities that are on the same map as player, including player
-}
mapCurrentEntities : (List ( Point, Entity entityData ) -> a) -> World tileData entityData -> a
mapCurrentEntities f (World world) =
    (world.player :: world.entities)
        |> List.filter (\e -> e.position.mapOffset == world.player.position.mapOffset)
        |> List.map (\e -> ( e.position.local, e ))
        |> f


getPlayer : World tileData entityData -> Entity entityData
getPlayer (World world) =
    world.player



-- MAP


{-| A map is a container for a hex grid and a name
-}
type alias Map tileData =
    { name : String
    , grid : HexGrid tileData
    }


newMap : String -> HexGrid tileData -> Map tileData
newMap name grid =
    Map name grid



-- ENTITY


{-| Entity position, local represents position relative to current map, mapOffset represents current map position
-}
type alias EntityPosition =
    { local : Point
    , mapOffset : Point
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
    , position : EntityPosition
    , state : EntityState
    , data : entityData
    }


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
                , position = { mapOffset = player.position.mapOffset, local = p }
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
