module HexEngine.HexMap exposing (HexMap, empty, insertTile)

import Dict exposing (Dict)
import HexEngine.Point exposing (Point)


type alias HexMap tile =
    Dict Point tile


empty : HexMap tile
empty =
    Dict.empty


{-| Insert tile, replace on collision
-}
insertTile : Point -> tile -> HexMap tile -> HexMap tile
insertTile point tile map =
    Dict.insert point tile map
