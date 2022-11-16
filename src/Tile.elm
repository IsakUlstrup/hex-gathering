module Tile exposing
    ( Tile(..)
    , isWalkable
    )

import Dict
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)


type Tile
    = Low
    | Medium
    | High


{-| determine if a given point is walkable

Only tiles that exist and are of variant Medium are walkable

Entities are not walkable

-}
isWalkable : HexMap Tile -> Point -> Bool
isWalkable map point =
    case Dict.get point map of
        Just Low ->
            False

        Just Medium ->
            True

        Just High ->
            False

        Nothing ->
            False
