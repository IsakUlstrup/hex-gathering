module HexEngine.HexMap exposing (HexMap, empty, insertTile)

import Dict exposing (Dict)
import HexEngine.Point exposing (Point)


type alias HexMap tile =
    { name : String
    , grid : Dict Point tile
    }


empty : String -> HexMap tile
empty name =
    HexMap name Dict.empty


{-| Insert hex, replace on collision
-}
insertTile : Point -> tile -> HexMap tile -> HexMap tile
insertTile point tile map =
    { map | grid = Dict.insert point tile map.grid }
