module HexEngine.World exposing
    ( Map
    , World
    , addEntity
    , addMap
    , getPlayer
    , getPoint
    , mapCurrentEntities
    , mapCurrentGrid
    , newMap
    , newWorld
    , playerMoveMap
    , updateEntities
    , updatePlayer
    )

import Dict exposing (Dict)
import HexEngine.Entity as Entity exposing (Entity, WorldPosition)
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
newWorld : Point -> Map tileData -> entityData -> World tileData entityData
newWorld mapPosition initMap playerData =
    World
        { entities = []
        , maps = Dict.fromList [ ( mapPosition, initMap ) ]
        , player = Entity.new 0 mapPosition ( 0, 0, 0 ) playerData
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
                    | entities = Entity.new world.idCounter mapOffset position entity :: world.entities
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
            World { world | player = Entity.setPosition (WorldPosition mapPosition localPosition) world.player }

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
