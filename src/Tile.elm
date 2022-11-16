module Tile exposing
    ( Entity(..)
    , Terrain(..)
    , Tile(..)
    , getEntity
    , isWalkable
    )

import Dict
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)


type Terrain
    = Low
    | Medium
    | High


type Entity
    = Resource Char
    | NPC Char
    | MapTransition Point


type Tile
    = Terrain Terrain
    | TerrainEntity Terrain Entity


{-| determine if a given point is walkable

Only tiles that exist and are of variant Medium are walkable

Entities are not walkable

-}
isWalkable : HexMap Tile -> Point -> Bool
isWalkable map point =
    case Dict.get point map of
        Just (Terrain t) ->
            case t of
                Medium ->
                    True

                _ ->
                    False

        Just (TerrainEntity _ _) ->
            False

        Nothing ->
            False


getEntity : Point -> HexMap Tile -> Maybe Entity
getEntity position map =
    case Dict.get position map of
        Just (TerrainEntity _ entity) ->
            Just entity

        _ ->
            Nothing
