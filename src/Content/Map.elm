module Content.Map exposing (testGrid, testGrid2, testGrid3)

import HexEngine.HexGrid as HexGrid exposing (HexGrid)
import Tile exposing (Tile(..))


testGrid : HexGrid Tile
testGrid =
    HexGrid.empty
        |> HexGrid.insertTile ( 0, 0, 0 ) Grass
        |> HexGrid.insertTile ( 0, 1, -1 ) Grass
        |> HexGrid.insertTile ( 1, 0, -1 ) Grass
        |> HexGrid.insertTile ( 1, 1, -2 ) Mountain
        |> HexGrid.insertTile ( -1, 1, 0 ) Water
        |> HexGrid.insertTile ( -1, 2, -1 ) Grass
        |> HexGrid.insertTile ( -1, 3, -2 ) Grass
        |> HexGrid.insertTile ( 0, 3, -3 ) Water
        |> HexGrid.insertTile ( 1, 2, -3 ) Water
        |> HexGrid.insertTile ( 1, 3, -4 ) Water
        |> HexGrid.insertTile ( 2, -1, -1 ) Grass
        |> HexGrid.insertTile ( 3, -2, -1 ) Grass
        |> HexGrid.insertTile ( 4, -2, -2 ) Grass
        |> HexGrid.insertTile ( 6, -3, -3 ) Mountain
        |> HexGrid.insertTile ( 5, -2, -3 ) Grass
        |> HexGrid.insertTile ( -4, 2, 2 ) Grass
        |> HexGrid.insertTile ( -5, 3, 2 ) Grass
        |> HexGrid.insertTile ( -5, 2, 3 ) Grass
        |> HexGrid.insertTile ( -4, 3, 1 ) Mountain
        |> HexGrid.insertTile ( 0, -1, 1 ) Water
        |> HexGrid.insertTile ( 1, -2, 1 ) Grass
        |> HexGrid.insertTile ( 1, -1, 0 ) Grass
        |> HexGrid.insertTile ( 0, -2, 2 ) Grass
        |> HexGrid.insertTile ( 0, -3, 3 ) Grass


testGrid2 : HexGrid Tile
testGrid2 =
    HexGrid.empty
        |> HexGrid.insertTile ( 0, 0, 0 ) Grass
        |> HexGrid.insertTile ( 0, 1, -1 ) Grass
        |> HexGrid.insertTile ( 1, 0, -1 ) Grass
        |> HexGrid.insertTile ( -1, 0, 1 ) Mountain
        |> HexGrid.insertTile ( -1, 1, 0 ) Mountain
        |> HexGrid.insertTile ( 0, -1, 1 ) Water
        |> HexGrid.insertTile ( 1, -1, 0 ) Water
        |> HexGrid.insertTile ( -1, 2, -1 ) Grass
        |> HexGrid.insertTile ( 1, 1, -2 ) Grass
        |> HexGrid.insertTile ( 1, 1, -2 ) Grass
        |> HexGrid.insertTile ( 2, -2, 0 ) Mountain
        |> HexGrid.insertTile ( 3, -2, -1 ) Water
        |> HexGrid.insertTile ( 2, -1, -1 ) Water


testGrid3 : HexGrid Tile
testGrid3 =
    HexGrid.empty
        |> HexGrid.insertTile ( 0, 0, 0 ) Grass
        |> HexGrid.insertTile ( 0, 1, -1 ) Grass
        |> HexGrid.insertTile ( 1, 0, -1 ) Grass
        |> HexGrid.insertTile ( 2, -1, -1 ) Grass
        |> HexGrid.insertTile ( -2, 3, -1 ) Water
        |> HexGrid.insertTile ( 1, 0, -1 ) Grass
        |> HexGrid.insertTile ( 4, -3, -1 ) Mountain
        |> HexGrid.insertTile ( 1, 0, -1 ) Grass
        |> HexGrid.insertTile ( -1, 2, -1 ) Grass
        |> HexGrid.insertTile ( 1, 1, -2 ) Grass
        |> HexGrid.insertTile ( 1, 1, -2 ) Grass
        |> HexGrid.insertTile ( 2, -2, 0 ) Mountain
        |> HexGrid.insertTile ( 2, 3, -5 ) Mountain
        |> HexGrid.insertTile ( 1, 2, -3 ) Water
