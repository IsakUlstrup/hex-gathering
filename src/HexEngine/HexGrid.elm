module HexEngine.HexGrid exposing (HexGrid, empty, get, insertTile, toList)

import Dict exposing (Dict)
import HexEngine.Point as Point exposing (Point)


type HexGrid tile
    = HexGrid (Dict Point tile)


empty : HexGrid tile
empty =
    HexGrid Dict.empty


{-| Insert tile, replace on collision
-}
insertTile : Point -> tile -> HexGrid tile -> HexGrid tile
insertTile position tile (HexGrid grid) =
    if Point.valid position then
        HexGrid <| Dict.insert position tile grid

    else
        HexGrid grid


toList : HexGrid tile -> List ( Point, tile )
toList (HexGrid grid) =
    grid
        |> Dict.toList


get : Point -> HexGrid tile -> Maybe tile
get position (HexGrid grid) =
    if Point.valid position then
        Dict.get position grid

    else
        Nothing
