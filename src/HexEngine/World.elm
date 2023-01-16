module HexEngine.World exposing
    ( World
    , addMap
    , filterMapGrids
    , getEntities
    , getMaps
    , getPlayer
    , getPlayerPosition
    , getPoint
    , newWorld
    , playerMoveMap
    , updateEntities
    , updatePlayer
    )

import Dict exposing (Dict)
import HexEngine.Entity as Entity exposing (Entity, WorldPosition)
import HexEngine.HexGrid as Grid exposing (HexGrid)
import HexEngine.Point as Point exposing (Point)



-- WORLD


{-| A world represents an entire gameState. Includes multiple maps and entities along side a player

Handles entity Ids automatically

-}
type World tileData entityData
    = World
        { entities : List (Entity entityData)
        , maps : Dict Point (HexGrid tileData)
        , player : Entity entityData
        , idCounter : Int
        }


{-| Create a new world with provided map and player. Both will be placed at (0, 0, 0)
-}
newWorld : Point -> HexGrid tileData -> ( Point, entityData ) -> List ( Point, entityData ) -> World tileData entityData
newWorld mapPosition initMap ( playerPosition, playerData ) entities =
    World
        { entities = []
        , maps = Dict.fromList [ ( mapPosition, initMap ) ]
        , player = Entity.new 0 mapPosition playerPosition playerData
        , idCounter = 1
        }
        |> addEntities mapPosition entities


{-| Add entity to world with generated id
-}
addEntity : Point -> Point -> entityData -> World tileData entityData -> World tileData entityData
addEntity mapOffset position entity (World world) =
    if Point.valid position then
        World
            { world
                | entities = Entity.new world.idCounter mapOffset position entity :: world.entities
                , idCounter = world.idCounter + 1
            }

    else
        World world


{-| add a list of entities to a map
-}
addEntities : Point -> List ( Point, entityData ) -> World tileData entityData -> World tileData entityData
addEntities mapPosition entities world =
    List.foldl (\( p, e ) -> addEntity mapPosition p e) world entities


{-| add new map to world, does nothing if a map exists at target position
-}
addMap : Point -> HexGrid tileData -> List ( Point, entityData ) -> World tileData entityData -> World tileData entityData
addMap position map entities (World world) =
    case Dict.get position world.maps of
        Just _ ->
            World world

        Nothing ->
            if Point.valid position then
                World { world | maps = Dict.insert position map world.maps }
                    |> addEntities position entities

            else
                World world


{-| Get map where player is located
-}
playerMap : World tileData entityData -> Maybe (HexGrid tileData)
playerMap (World world) =
    Dict.get (Entity.getPosition world.player).map world.maps


{-| Get tile and entity at given position in active map
-}
getPoint : Point -> World tileData entityData -> ( Maybe tileData, Maybe (Entity entityData) )
getPoint target (World world) =
    ( playerMap (World world)
        |> Maybe.andThen (Grid.get target)
    , world.entities
        |> List.filter (\e -> (Entity.getPosition e).local == target && (Entity.getPosition e).map == (Entity.getPosition world.player).map)
        |> List.head
    )


{-| get player
-}
getPlayer : World tileData entityData -> Entity entityData
getPlayer (World world) =
    world.player


{-| Get world maps
-}
getMaps : World tileData entityData -> Dict Point (HexGrid tileData)
getMaps (World world) =
    world.maps


getEntities : World tileData entityData -> List (Entity entityData)
getEntities (World world) =
    world.player :: world.entities


{-| Get player position
-}
getPlayerPosition : World tileData entityData -> WorldPosition
getPlayerPosition (World world) =
    Entity.getPosition world.player


{-| update player with provided function
-}
updatePlayer : (Entity entityData -> Entity entityData) -> World tileData entityData -> World tileData entityData
updatePlayer f (World world) =
    World { world | player = f world.player }


{-| updates all entities in the world with provided function
-}
updateEntities : (Entity entityData -> Entity entityData) -> World tileData entityData -> World tileData entityData
updateEntities f (World world) =
    World
        { world
            | player = f world.player
            , entities = List.map f world.entities
        }


{-| filterMap all grids in the world with provided function
-}
filterMapGrids : (Point -> List ( Point, tileData ) -> Maybe a) -> World tileData entityData -> List a
filterMapGrids f (World world) =
    world.maps
        |> Dict.toList
        |> List.map (Tuple.mapSecond Grid.toList)
        |> List.filterMap (\( pos, grid ) -> f pos grid)


{-| Move player to another map, does nothing if map doesn't exist
-}
playerMoveMap : Int -> Point -> Point -> World tileData entityData -> World tileData entityData
playerMoveMap moveDuration mapPosition localPosition (World world) =
    case Dict.get mapPosition world.maps of
        Just _ ->
            World
                { world | player = Entity.mapTransition moveDuration mapPosition localPosition world.player }

        Nothing ->
            World world
