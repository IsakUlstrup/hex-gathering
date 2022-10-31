module Content.Maps exposing (errorMap, testMap, testMap2, testMap3)

import HexEngine.HexEntityMap as HexEntityMap exposing (HexEntityMap)
import HexEngine.HexMap as HexMap exposing (HexMap)
import Tile exposing (Entity(..), Tile(..))


testMap : HexMap Tile
testMap =
    HexMap.empty
        |> HexMap.insertReplaceHex ( ( 0, 0, 0 ), Medium )
        |> HexMap.insertReplaceHex ( ( 0, 1, -1 ), Medium )
        |> HexMap.insertReplaceHex ( ( 1, 0, -1 ), Medium )
        |> HexMap.insertReplaceHex ( ( 1, 1, -2 ), High )
        |> HexMap.insertReplaceHex ( ( -1, 1, 0 ), Low )
        |> HexMap.insertReplaceHex ( ( -1, 2, -1 ), Medium )
        |> HexMap.insertReplaceHex ( ( -1, 3, -2 ), Medium )
        |> HexMap.insertReplaceHex ( ( 0, 3, -3 ), Low )
        |> HexMap.insertReplaceHex ( ( 1, 2, -3 ), Low )
        |> HexMap.insertReplaceHex ( ( 1, 3, -4 ), Low )
        |> HexMap.insertReplaceHex ( ( 2, -1, -1 ), Medium )
        |> HexMap.insertReplaceHex ( ( 3, -2, -1 ), Medium )
        |> HexMap.insertReplaceHex ( ( 4, -2, -2 ), Medium )
        |> HexMap.insertReplaceHex ( ( 6, -3, -3 ), High )
        |> HexMap.insertReplaceHex ( ( 5, -2, -3 ), Medium )
        |> HexMap.insertReplaceHex ( ( -4, 2, 2 ), Medium )
        |> HexMap.insertReplaceHex ( ( -5, 3, 2 ), Medium )
        |> HexMap.insertReplaceHex ( ( -5, 2, 3 ), Medium )
        |> HexMap.insertReplaceHex ( ( -4, 3, 1 ), High )
        |> HexMap.insertReplaceHex ( ( 0, -1, 1 ), Low )
        |> HexMap.insertReplaceHex ( ( 1, -2, 1 ), Medium )
        |> HexMap.insertReplaceHex ( ( 1, -1, 0 ), Medium )
        |> HexMap.insertReplaceHex ( ( 0, -2, 2 ), Medium )
        |> HexMap.insertReplaceHex ( ( 0, -3, 3 ), Medium )


testMap2 : ( String, HexEntityMap Tile Entity )
testMap2 =
    ( "Home"
    , HexEntityMap.empty
        |> HexEntityMap.insertTile Medium ( 0, 0, 0 )
        |> HexEntityMap.insertTile Medium ( 0, 1, -1 )
        |> HexEntityMap.insertTile Medium ( 1, 0, -1 )
        |> HexEntityMap.insertTile Medium ( 2, 0, -2 )
        |> HexEntityMap.insertTile High ( 1, 1, -2 )
        |> HexEntityMap.insertTile Low ( -1, 1, 0 )
        |> HexEntityMap.insertTile Medium ( -1, 2, -1 )
        |> HexEntityMap.insertTile Medium ( -1, 3, -2 )
        |> HexEntityMap.insertTile Low ( 0, 3, -3 )
        |> HexEntityMap.insertTile Low ( 1, 2, -3 )
        |> HexEntityMap.insertTile Low ( 1, 3, -4 )
        |> HexEntityMap.insertTile Medium ( 2, -1, -1 )
        |> HexEntityMap.insertTile Medium ( 3, -2, -1 )
        |> HexEntityMap.insertTile Medium ( 4, -2, -2 )
        |> HexEntityMap.insertTile High ( 6, -3, -3 )
        |> HexEntityMap.insertTile Medium ( 5, -2, -3 )
        |> HexEntityMap.insertTile Medium ( -4, 2, 2 )
        |> HexEntityMap.insertTile Medium ( -5, 3, 2 )
        |> HexEntityMap.insertTile Medium ( -5, 2, 3 )
        |> HexEntityMap.insertTile High ( -4, 3, 1 )
        |> HexEntityMap.insertTile Low ( 0, -1, 1 )
        |> HexEntityMap.insertTile Medium ( 1, -2, 1 )
        |> HexEntityMap.insertTile Medium ( 1, -1, 0 )
        |> HexEntityMap.insertTile Medium ( 0, -2, 2 )
        |> HexEntityMap.insertTile Medium ( 0, -3, 3 )
        |> HexEntityMap.insertEntity (Resource '🌲') ( -5, 3, 2 )
        |> HexEntityMap.insertEntity (Resource '🌴') ( -5, 2, 3 )
        |> HexEntityMap.insertEntity (Resource '🌴') ( -5, 2, 3 )
        |> HexEntityMap.insertEntity (Resource '🌴') ( 1, -2, 1 )
        |> HexEntityMap.insertEntity (NPC '🧙') ( 2, 0, -2 )
    )


testMap3 : ( String, HexEntityMap Tile Entity )
testMap3 =
    ( "Mine"
    , HexEntityMap.empty
        |> HexEntityMap.insertTile Medium ( 0, 0, 0 )
        |> HexEntityMap.insertTile Medium ( -1, 1, 0 )
        |> HexEntityMap.insertTile Medium ( -2, 1, 1 )
        |> HexEntityMap.insertTile Medium ( -1, 0, 1 )
    )


errorMap : HexEntityMap Tile Entity
errorMap =
    HexEntityMap.empty
        |> HexEntityMap.insertTile Medium ( 0, 0, 0 )
