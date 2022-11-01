module Content.Maps exposing (errorMap, testMap, testMap3)

import HexEngine.HexMap as HexMap exposing (HexMap)
import Tile exposing (Entity(..), Terrain(..), Tile(..))


testMap : HexMap Tile
testMap =
    HexMap.empty "Home"
        |> HexMap.insertTile ( 0, 0, 0 ) (Terrain Medium)
        |> HexMap.insertTile ( 0, 1, -1 ) (Terrain Medium)
        |> HexMap.insertTile ( 1, 0, -1 ) (Terrain Medium)
        |> HexMap.insertTile ( 1, 1, -2 ) (Terrain High)
        |> HexMap.insertTile ( -1, 1, 0 ) (Terrain Low)
        |> HexMap.insertTile ( -1, 2, -1 ) (Terrain Medium)
        |> HexMap.insertTile ( -1, 3, -2 ) (TerrainEntity Medium (NPC '🦖'))
        |> HexMap.insertTile ( 0, 3, -3 ) (Terrain Low)
        |> HexMap.insertTile ( 1, 2, -3 ) (Terrain Low)
        |> HexMap.insertTile ( 1, 3, -4 ) (Terrain Low)
        |> HexMap.insertTile ( 2, -1, -1 ) (Terrain Medium)
        |> HexMap.insertTile ( 3, -2, -1 ) (Terrain Medium)
        |> HexMap.insertTile ( 4, -2, -2 ) (Terrain Medium)
        |> HexMap.insertTile ( 6, -3, -3 ) (Terrain High)
        |> HexMap.insertTile ( 5, -2, -3 ) (TerrainEntity Medium (MapTransition "Mine"))
        |> HexMap.insertTile ( -4, 2, 2 ) (Terrain Medium)
        |> HexMap.insertTile ( -5, 3, 2 ) (Terrain Medium)
        |> HexMap.insertTile ( -5, 2, 3 ) (Terrain Medium)
        |> HexMap.insertTile ( -4, 3, 1 ) (Terrain High)
        |> HexMap.insertTile ( 0, -1, 1 ) (Terrain Low)
        |> HexMap.insertTile ( 1, -2, 1 ) (Terrain Medium)
        |> HexMap.insertTile ( 1, -1, 0 ) (Terrain Medium)
        |> HexMap.insertTile ( 0, -2, 2 ) (TerrainEntity Medium (Resource '🌲'))
        |> HexMap.insertTile ( 0, -3, 3 ) (Terrain Medium)



-- testMap2 : ( String, HexEntityMap Tile Entity )
-- testMap2 =
--     ( "Home"
--     , HexEntityMap.empty
--         |> HexEntityMap.insertTile Medium ( 0, 0, 0 )
--         |> HexEntityMap.insertTile Medium ( 0, 1, -1 )
--         |> HexEntityMap.insertTile Medium ( 1, 0, -1 )
--         |> HexEntityMap.insertTile Medium ( 2, 0, -2 )
--         |> HexEntityMap.insertTile High ( 1, 1, -2 )
--         |> HexEntityMap.insertTile Low ( -1, 1, 0 )
--         |> HexEntityMap.insertTile Medium ( -1, 2, -1 )
--         |> HexEntityMap.insertTile Medium ( -1, 3, -2 )
--         |> HexEntityMap.insertTile Low ( 0, 3, -3 )
--         |> HexEntityMap.insertTile Low ( 1, 2, -3 )
--         |> HexEntityMap.insertTile Low ( 1, 3, -4 )
--         |> HexEntityMap.insertTile Medium ( 2, -1, -1 )
--         |> HexEntityMap.insertTile Medium ( 3, -2, -1 )
--         |> HexEntityMap.insertTile Medium ( 4, -2, -2 )
--         |> HexEntityMap.insertTile High ( 6, -3, -3 )
--         |> HexEntityMap.insertTile Medium ( 5, -2, -3 )
--         |> HexEntityMap.insertEntity (MapTransition "Mine") ( 5, -2, -3 )
--         |> HexEntityMap.insertTile Medium ( -4, 2, 2 )
--         |> HexEntityMap.insertTile Medium ( -5, 3, 2 )
--         |> HexEntityMap.insertTile Medium ( -5, 2, 3 )
--         |> HexEntityMap.insertTile High ( -4, 3, 1 )
--         |> HexEntityMap.insertTile Low ( 0, -1, 1 )
--         |> HexEntityMap.insertTile Medium ( 1, -2, 1 )
--         |> HexEntityMap.insertTile Medium ( 1, -1, 0 )
--         |> HexEntityMap.insertTile Medium ( 0, -2, 2 )
--         |> HexEntityMap.insertTile Medium ( 0, -3, 3 )
--         |> HexEntityMap.insertEntity (Resource '🌲') ( -5, 3, 2 )
--         |> HexEntityMap.insertEntity (Resource '🌴') ( -5, 2, 3 )
--         |> HexEntityMap.insertEntity (Resource '🌴') ( -5, 2, 3 )
--         |> HexEntityMap.insertEntity (Resource '🌴') ( 1, -2, 1 )
--         |> HexEntityMap.insertEntity (NPC '🦖') ( 2, 0, -2 )
--     )


testMap3 : HexMap Tile
testMap3 =
    HexMap.empty "Mine"
        |> HexMap.insertTile ( 0, 0, 0 ) (Terrain Medium)
        |> HexMap.insertTile ( -1, 1, 0 ) (Terrain Medium)
        |> HexMap.insertTile ( -2, 1, 1 ) (Terrain Medium)
        |> HexMap.insertTile ( -1, 0, 1 ) (TerrainEntity Medium (MapTransition "Home"))


errorMap : HexMap Tile
errorMap =
    HexMap.empty "Error"
        |> HexMap.insertTile ( 0, 0, 0 ) (Terrain Medium)
