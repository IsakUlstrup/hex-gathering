module Tile exposing
    ( Entity(..)
    , Terrain(..)
    , Tile(..)
    )


type Terrain
    = Low
    | Medium
    | High


type Entity
    = Resource Char
    | NPC Char
    | MapTransition String


type Tile
    = Terrain Terrain
    | TerrainEntity Terrain Entity
