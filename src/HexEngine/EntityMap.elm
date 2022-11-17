module HexEngine.EntityMap exposing
    ( EntityMap
    , addEntity
    , entityList
    , getPoint
    , gridList
    , new
    , updateEntity
    )

import HexEngine.HexGrid as HexGrid exposing (HexGrid)
import HexEngine.Point exposing (Point)


type EntityMap tile entity
    = EntityMap
        { name : String
        , grid : HexGrid tile
        , entities : List (Entity entity)
        , idCounter : Int
        }


type alias Entity entity =
    { position : Point
    , data : entity
    , id : Int
    }


new : String -> HexGrid tile -> EntityMap tile entity
new name grid =
    EntityMap
        { name = name
        , grid = grid
        , entities = []
        , idCounter = 0
        }


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
            | entities = Entity position entity map.idCounter :: map.entities
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


updateData : (entity -> entity) -> Entity entity -> Entity entity
updateData f entity =
    { entity | data = f entity.data }


updateById : Int -> (entity -> entity) -> Entity entity -> Entity entity
updateById target f entity =
    if entity.id == target then
        updateData f entity

    else
        entity
