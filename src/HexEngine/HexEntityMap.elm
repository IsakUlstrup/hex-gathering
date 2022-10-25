module HexEngine.HexEntityMap exposing (Entity, HexEntityMap)

import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)


type alias Entity entity =
    { position : Point
    , data : entity
    }


type alias HexEntityMap tile entity =
    { map : HexMap tile
    , entities : List (Entity entity)
    }
