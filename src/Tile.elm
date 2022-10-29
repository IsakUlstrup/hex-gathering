module Tile exposing (Entity(..), Tile(..))


type Tile
    = Low
    | Medium
    | High


type Entity
    = Resource Char
