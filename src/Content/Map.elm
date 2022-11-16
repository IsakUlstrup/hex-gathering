module Content.Map exposing (testMap)

import HexEngine.HexMap as HexMap
import HexEngine.Point exposing (Point)
import Island exposing (Island, IslandMap)
import Tile exposing (Tile(..))


testIsland : ( Point, Island Tile )
testIsland =
    ( ( 0, 0, 0 )
    , Island.new "Home"
        (HexMap.empty
            |> HexMap.insertTile ( 0, 0, 0 ) Medium
            |> HexMap.insertTile ( 0, 1, -1 ) Medium
            |> HexMap.insertTile ( 1, 0, -1 ) Medium
            |> HexMap.insertTile ( 1, 1, -2 ) High
            |> HexMap.insertTile ( -1, 1, 0 ) Low
            |> HexMap.insertTile ( -1, 2, -1 ) Medium
            |> HexMap.insertTile ( -1, 3, -2 ) Medium
            |> HexMap.insertTile ( 0, 3, -3 ) Low
            |> HexMap.insertTile ( 1, 2, -3 ) Low
            |> HexMap.insertTile ( 1, 3, -4 ) Low
            |> HexMap.insertTile ( 2, -1, -1 ) Medium
            |> HexMap.insertTile ( 3, -2, -1 ) Medium
            |> HexMap.insertTile ( 4, -2, -2 ) Medium
            |> HexMap.insertTile ( 6, -3, -3 ) High
            |> HexMap.insertTile ( 5, -2, -3 ) Medium
            |> HexMap.insertTile ( -4, 2, 2 ) Medium
            |> HexMap.insertTile ( -5, 3, 2 ) Medium
            |> HexMap.insertTile ( -5, 2, 3 ) Medium
            |> HexMap.insertTile ( -4, 3, 1 ) High
            |> HexMap.insertTile ( 0, -1, 1 ) Low
            |> HexMap.insertTile ( 1, -2, 1 ) Medium
            |> HexMap.insertTile ( 1, -1, 0 ) Medium
            |> HexMap.insertTile ( 0, -2, 2 ) Medium
            |> HexMap.insertTile ( 0, -3, 3 ) Medium
        )
    )


testIsland2 : ( Point, Island Tile )
testIsland2 =
    ( ( 0, 1, -1 )
    , Island.new "Mine"
        (HexMap.empty
            |> HexMap.insertTile ( 0, 0, 0 ) Medium
            |> HexMap.insertTile ( -1, 1, 0 ) Medium
            |> HexMap.insertTile ( -2, 1, 1 ) Medium
            |> HexMap.insertTile ( 0, 1, -1 ) Medium
            |> HexMap.insertTile ( 0, 2, -2 ) Medium
            |> HexMap.insertTile ( -1, 3, -2 ) Medium
            |> HexMap.insertTile ( -3, 2, 1 ) High
            |> HexMap.insertTile ( -2, 2, 0 ) High
            |> HexMap.insertTile ( -1, 0, 1 ) Low
        )
    )


testMap : IslandMap Tile
testMap =
    Island.newMap testIsland [ testIsland2 ]
