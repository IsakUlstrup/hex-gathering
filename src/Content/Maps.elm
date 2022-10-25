module Content.Maps exposing (testMap)

import HexEngine.HexMap as HexMap exposing (HexMap)
import Tile exposing (Tile(..))


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
