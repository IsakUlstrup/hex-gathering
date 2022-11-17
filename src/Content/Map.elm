module Content.Map exposing (testIsland, testIsland2)

import Entity exposing (Entity)
import HexEngine.EntityMap exposing (EntityMap)
import HexEngine.HexGrid as HexGrid
import HexEngine.Point exposing (Point)
import Tile exposing (Tile(..))


testIsland : ( Point, EntityMap Tile Entity )
testIsland =
    ( ( 0, 0, 0 )
    , HexEngine.EntityMap.new "Home"
        (HexGrid.empty
            |> HexGrid.insertTile ( 0, 0, 0 ) Medium
            |> HexGrid.insertTile ( 0, 1, -1 ) Medium
            |> HexGrid.insertTile ( 1, 0, -1 ) Medium
            |> HexGrid.insertTile ( 1, 1, -2 ) High
            |> HexGrid.insertTile ( -1, 1, 0 ) Low
            |> HexGrid.insertTile ( -1, 2, -1 ) Medium
            |> HexGrid.insertTile ( -1, 3, -2 ) Medium
            |> HexGrid.insertTile ( 0, 3, -3 ) Low
            |> HexGrid.insertTile ( 1, 2, -3 ) Low
            |> HexGrid.insertTile ( 1, 3, -4 ) Low
            |> HexGrid.insertTile ( 2, -1, -1 ) Medium
            |> HexGrid.insertTile ( 3, -2, -1 ) Medium
            |> HexGrid.insertTile ( 4, -2, -2 ) Medium
            |> HexGrid.insertTile ( 6, -3, -3 ) High
            |> HexGrid.insertTile ( 5, -2, -3 ) Medium
            |> HexGrid.insertTile ( -4, 2, 2 ) Medium
            |> HexGrid.insertTile ( -5, 3, 2 ) Medium
            |> HexGrid.insertTile ( -5, 2, 3 ) Medium
            |> HexGrid.insertTile ( -4, 3, 1 ) High
            |> HexGrid.insertTile ( 0, -1, 1 ) Low
            |> HexGrid.insertTile ( 1, -2, 1 ) Medium
            |> HexGrid.insertTile ( 1, -1, 0 ) Medium
            |> HexGrid.insertTile ( 0, -2, 2 ) Medium
            |> HexGrid.insertTile ( 0, -3, 3 ) Medium
        )
        |> HexEngine.EntityMap.addEntity ( 3, -2, -1 ) '🌴'
        |> HexEngine.EntityMap.addEntity ( 0, -3, 3 ) '🌺'
    )


testIsland2 : ( Point, EntityMap Tile Entity )
testIsland2 =
    ( ( 0, 1, -1 )
    , HexEngine.EntityMap.new "Mine"
        (HexGrid.empty
            |> HexGrid.insertTile ( 0, 0, 0 ) Medium
            |> HexGrid.insertTile ( -1, 1, 0 ) Medium
            |> HexGrid.insertTile ( -2, 1, 1 ) Medium
            |> HexGrid.insertTile ( 0, 1, -1 ) Medium
            |> HexGrid.insertTile ( 0, 2, -2 ) Medium
            |> HexGrid.insertTile ( -1, 3, -2 ) Medium
            |> HexGrid.insertTile ( -3, 2, 1 ) High
            |> HexGrid.insertTile ( -2, 2, 0 ) High
            |> HexGrid.insertTile ( -1, 0, 1 ) Low
        )
    )
