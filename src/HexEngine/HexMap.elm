module HexEngine.HexMap exposing (HexMap, empty, insertReplaceHex, mapHexes)

import Dict exposing (Dict)
import HexEngine.Point exposing (Point)


type alias HexMap tile =
    Dict Point tile


empty : HexMap tile
empty =
    [] |> Dict.fromList


mapHexes : (( Point, tile ) -> a) -> HexMap tile -> List a
mapHexes f map =
    Dict.toList map |> List.map f


{-| Insert hex, replace on collision
-}
insertReplaceHex : ( Point, tile ) -> HexMap tile -> HexMap tile
insertReplaceHex ( point, tile ) map =
    Dict.insert point tile map
