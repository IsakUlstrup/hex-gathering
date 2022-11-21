module HexEngine.World exposing
    ( Entity
    , EntityState
    , Map
    , World
    , WorldPosition
    , addEntity
    , addMap
    , findPath
    , findPathAdjacent
    , getPlayer
    , getPoint
    , mapCurrentEntities
    , mapCurrentGrid
    , move
    , newMap
    , newWorld
    , playerMoveMap
    , stateString
    , tickCooldown
    , updateEntities
    , updatePlayer
    )

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
newWorld : Point -> Map tileData -> entityData -> World tileData entityData
newWorld mapPosition initMap playerData =
    World
        { entities = []
        , maps = Dict.fromList [ ( mapPosition, initMap ) ]
        , player = Entity 0 (WorldPosition mapPosition ( 0, 0, 0 )) Idle playerData
        , idCounter = 1
        }


{-| Add entity to world with generated id
-}
addEntity : Point -> Point -> entityData -> World tileData entityData -> World tileData entityData
addEntity mapOffset position entity (World world) =
    case Dict.get mapOffset world.maps of
        Just _ ->
            World
                { world
                    | entities = Entity world.idCounter (WorldPosition mapOffset position) Idle entity :: world.entities
                    , idCounter = world.idCounter + 1
                }

        Nothing ->
            World world


{-| add new map to world, does nothing if a map exists at target position
-}
addMap : Point -> Map tileData -> World tileData entityData -> World tileData entityData
addMap position map (World world) =
    case Dict.get position world.maps of
        Just _ ->
            World world

        Nothing ->
            World { world | maps = Dict.insert position map world.maps }


{-| Get map where player is located
-}
playerMap : World tileData entityData -> Maybe (Map tileData)
playerMap (World world) =
    Dict.get world.player.position.map world.maps


{-| Get tile and entity at given position in active map
-}
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
mapCurrentGrid : (List ( WorldPosition, tileData ) -> a) -> World tileData entityData -> a
mapCurrentGrid f (World world) =
    case Dict.get world.player.position.map world.maps of
        Just map ->
            map.grid
                |> Grid.toList
                |> List.map (\( p, t ) -> ( WorldPosition world.player.position.map p, t ))
                |> f

        Nothing ->
            f []


{-| Map Entities that are on the same map as player, including player
-}
mapCurrentEntities : (List ( WorldPosition, Entity entityData ) -> a) -> World tileData entityData -> a
mapCurrentEntities f (World world) =
    (world.player :: world.entities)
        |> List.filter (\e -> e.position.map == world.player.position.map)
        |> List.map (\e -> ( e.position, e ))
        |> f


getPlayer : World tileData entityData -> Entity entityData
getPlayer (World world) =
    world.player


{-| Move player to another map, does nothing if map doesn't exist
-}
playerMoveMap : Point -> Point -> World tileData entityData -> World tileData entityData
playerMoveMap mapPosition localPosition (World world) =
    case Dict.get mapPosition world.maps of
        Just _ ->
            World { world | player = setPosition (WorldPosition mapPosition localPosition) world.player }

        Nothing ->
            World world



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


setPosition : WorldPosition -> Entity entityData -> Entity entityData
setPosition position entity =
    { entity | position = position }


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
