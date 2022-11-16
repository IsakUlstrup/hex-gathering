module Content.Islands exposing (testMap, testMap3)

import Content.Entities as Entities
import HexEngine.HexMap as HexMap
import HexEngine.Point exposing (Point)
import Island exposing (Island)
import Tile exposing (Terrain(..), Tile(..))


testMap : ( Point, Island Tile )
testMap =
    ( ( 0, 0, 0 )
    , Island.new "Home"
        (HexMap.empty
            |> HexMap.insertTile ( 0, 0, 0 ) (Terrain Medium)
            |> HexMap.insertTile ( 0, 1, -1 ) (Terrain Medium)
            |> HexMap.insertTile ( 1, 0, -1 ) (Terrain Medium)
            |> HexMap.insertTile ( 1, 1, -2 ) (Terrain High)
            |> HexMap.insertTile ( -1, 1, 0 ) (Terrain Low)
            |> HexMap.insertTile ( -1, 2, -1 ) (Terrain Medium)
            |> HexMap.insertTile ( -1, 3, -2 ) (TerrainEntity Medium Entities.awesomesaurus)
            |> HexMap.insertTile ( 0, 3, -3 ) (Terrain Low)
            |> HexMap.insertTile ( 1, 2, -3 ) (Terrain Low)
            |> HexMap.insertTile ( 1, 3, -4 ) (Terrain Low)
            |> HexMap.insertTile ( 2, -1, -1 ) (Terrain Medium)
            |> HexMap.insertTile ( 3, -2, -1 ) (Terrain Medium)
            |> HexMap.insertTile ( 4, -2, -2 ) (Terrain Medium)
            |> HexMap.insertTile ( 6, -3, -3 ) (Terrain High)
            |> HexMap.insertTile ( 5, -2, -3 ) (TerrainEntity Medium (Entities.mapTransition ( 0, 1, -1 )))
            |> HexMap.insertTile ( -4, 2, 2 ) (Terrain Medium)
            |> HexMap.insertTile ( -5, 3, 2 ) (Terrain Medium)
            |> HexMap.insertTile ( -5, 2, 3 ) (Terrain Medium)
            |> HexMap.insertTile ( -4, 3, 1 ) (Terrain High)
            |> HexMap.insertTile ( 0, -1, 1 ) (Terrain Low)
            |> HexMap.insertTile ( 1, -2, 1 ) (Terrain Medium)
            |> HexMap.insertTile ( 1, -1, 0 ) (Terrain Medium)
            |> HexMap.insertTile ( 0, -2, 2 ) (TerrainEntity Medium Entities.evergreen)
            |> HexMap.insertTile ( 0, -3, 3 ) (Terrain Medium)
        )
    )


testMap3 : ( Point, Island Tile )
testMap3 =
    ( ( 0, 1, -1 )
    , Island.new "Mine"
        (HexMap.empty
            |> HexMap.insertTile ( 0, 0, 0 ) (Terrain Medium)
            |> HexMap.insertTile ( -1, 1, 0 ) (Terrain Medium)
            |> HexMap.insertTile ( -2, 1, 1 ) (Terrain Medium)
            |> HexMap.insertTile ( 0, 1, -1 ) (Terrain Medium)
            |> HexMap.insertTile ( 0, 2, -2 ) (Terrain Medium)
            |> HexMap.insertTile ( -1, 3, -2 ) (TerrainEntity Medium (Entities.mapTransition ( 0, 0, 0 )))
            |> HexMap.insertTile ( -3, 2, 1 ) (Terrain High)
            |> HexMap.insertTile ( -2, 2, 0 ) (Terrain High)
            |> HexMap.insertTile ( -1, 0, 1 ) (Terrain Low)
        )
    )
