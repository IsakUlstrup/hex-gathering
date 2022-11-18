module HexEngine.EntityMap exposing
    ( EntityMap
    , addEntity
    , entityList
    , getOffset
    , getPoint
    , gridList
    , new
    , updateEntity
    )

import HexEngine.HexGrid as HexGrid exposing (HexGrid)
import HexEngine.Point exposing (Point)
import Set


type EntityMap tile entity
    = EntityMap
        { name : String
        , grid : HexGrid tile
        , entities : List (Entity entity)
        , idCounter : Int
        , offset : Point
        }


new : String -> HexGrid tile -> Point -> EntityMap tile entity
new name grid offset =
    EntityMap
        { name = name
        , grid = grid
        , entities = []
        , idCounter = 0
        , offset = offset
        }


getOffset : EntityMap tile entity -> Point
getOffset (EntityMap map) =
    map.offset


entityList : EntityMap tile entity -> List ( Point, entity )
entityList (EntityMap map) =
    map.entities
        |> List.map (\e -> ( e.position, e.data ))


gridList : EntityMap tile entity -> List ( Point, tile )
gridList (EntityMap map) =
    map.grid
        |> HexGrid.toList


addEntity : Point -> entity -> EntityMap tile entity -> EntityMap tile entity
addEntity position entity (EntityMap map) =
    EntityMap
        { map
            | entities = Entity position entity map.idCounter Idle :: map.entities
            , idCounter = map.idCounter + 1
        }


getPoint : Point -> EntityMap tile entity -> ( Maybe entity, Maybe tile )
getPoint position (EntityMap map) =
    ( map.entities
        |> List.filterMap
            (\e ->
                if e.position == position then
                    Just e.data

                else
                    Nothing
            )
        |> List.head
    , HexGrid.get position map.grid
    )


updateEntity : Int -> (entity -> entity) -> EntityMap tile entity -> EntityMap tile entity
updateEntity id f (EntityMap map) =
    EntityMap
        { map
            | entities =
                map.entities
                    |> List.map
                        (updateById id f)
        }



-- ENTITY


type alias Entity entity =
    { position : Point
    , data : entity
    , id : Int
    , state : EntityState
    }


type EntityState
    = Moving (List Point) Int
    | Cooling (List Point)
    | BlockedPath Int
    | Idle


updateData : (entity -> entity) -> Entity entity -> Entity entity
updateData f entity =
    { entity | data = f entity.data }


updateById : Int -> (entity -> entity) -> Entity entity -> Entity entity
updateById target f entity =
    if entity.id == target then
        updateData f entity

    else
        entity


stateString : Entity entity -> String
stateString entity =
    case entity.state of
        Moving _ _ ->
            "moving"

        Cooling _ ->
            "cooling"

        BlockedPath _ ->
            "blocked"

        Idle ->
            "idle"


setPath : List Point -> Entity entity -> Entity entity
setPath path entity =
    case entity.state of
        Moving _ cd ->
            { entity | state = Moving path cd }

        Cooling _ ->
            { entity | state = Cooling path }

        BlockedPath _ ->
            { entity | state = Cooling path }

        Idle ->
            { entity | state = Cooling path }


findPath : (Point -> Bool) -> Point -> Entity entity -> Entity entity
findPath walkable to entity =
    case HexEngine.Point.pathfind walkable entity.position to of
        Just path ->
            setPath path entity

        Nothing ->
            { entity | state = BlockedPath 200 }


{-| find shortest path to a tile adjacent to target tile
-}
findPathAdjacent : (Point -> Bool) -> Point -> Entity entity -> Entity entity
findPathAdjacent walkable to entity =
    HexEngine.Point.neighbors to
        |> Set.toList
        |> List.filterMap (HexEngine.Point.pathfind walkable entity.position)
        |> List.sortBy List.length
        |> List.head
        |> (\p ->
                case p of
                    Just path ->
                        setPath path entity

                    Nothing ->
                        { entity | state = BlockedPath 200 }
           )


tickCooldown : Int -> Entity entity -> Entity entity
tickCooldown dt entity =
    case entity.state of
        Moving path cd ->
            { entity | state = Moving path (max 0 (cd - dt)) }

        Cooling path ->
            { entity | state = Cooling path }

        BlockedPath cd ->
            { entity | state = BlockedPath (max 0 (cd - dt)) }

        Idle ->
            entity


move : Int -> Entity entity -> Entity entity
move moveTime entity =
    case entity.state of
        Moving path cd ->
            if cd == 0 then
                { entity | state = Cooling path }

            else
                entity

        Cooling (p :: path) ->
            { entity
                | state = Moving path moveTime
                , position = p
            }

        Cooling [] ->
            { entity
                | state = Idle
            }

        BlockedPath cd ->
            if cd <= 0 then
                { entity
                    | state = Idle
                }

            else
                entity

        Idle ->
            entity


isIdle : Entity entity -> Bool
isIdle entity =
    case entity.state of
        Idle ->
            True

        _ ->
            False


hasPath : Entity entity -> Bool
hasPath entity =
    case entity.state of
        Idle ->
            False

        BlockedPath _ ->
            False

        Moving _ _ ->
            True

        Cooling _ ->
            True


readyToInteract : Entity entity -> Point -> Bool
readyToInteract entity point =
    HexEngine.Point.distance point entity.position == 1 && isIdle entity


resetPosition : Entity entity -> Entity entity
resetPosition entity =
    { entity | position = ( 0, 0, 0 ) }
