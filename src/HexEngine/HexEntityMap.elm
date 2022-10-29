module HexEngine.HexEntityMap exposing (HexEntityMap, empty, insertEntity, insertTile)

import Dict
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)


type alias HexEntityMap tile entity =
    { tiles : HexMap tile
    , entities : HexMap entity
    }


empty : HexEntityMap tile entity
empty =
    HexEntityMap Dict.empty Dict.empty


insertTile : tile -> Point -> HexEntityMap tile entity -> HexEntityMap tile entity
insertTile tile position map =
    { map | tiles = Dict.insert position tile map.tiles }


insertEntity : entity -> Point -> HexEntityMap tile entity -> HexEntityMap tile entity
insertEntity entity position map =
    { map | entities = Dict.insert position entity map.entities }
