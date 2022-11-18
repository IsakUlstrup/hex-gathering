module HexEngine.World exposing (Entity, EntityPosition, EntityState, Map, World, addEntity, getPlayer, mapCurrentEntities, mapCurrentGrid, newMap, newWorld)

import Dict exposing (Dict)
import HexEngine.HexGrid as Grid exposing (HexGrid)
import HexEngine.Point exposing (Point)



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


{-| Map the grid player is located in
-}
mapCurrentGrid : World tileData entityData -> (List ( Point, tileData ) -> a) -> a
mapCurrentGrid (World world) f =
    case Dict.get world.player.position.mapOffset world.maps of
        Just map ->
            f (Grid.toList map.grid)

        Nothing ->
            f []


{-| Map Entities that are on the same map as player, including player
-}
mapCurrentEntities : World tileData entityData -> (List ( Point, Entity entityData ) -> a) -> a
mapCurrentEntities (World world) f =
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
    = Idle


{-| Entity wraps around provied entityData and adds position, state and a unique id
-}
type alias Entity entityData =
    { id : Int
    , position : EntityPosition
    , state : EntityState
    , data : entityData
    }
