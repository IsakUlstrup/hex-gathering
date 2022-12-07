module Tile exposing (Tile(..), tileToString)


type Tile
    = Low
    | Medium
    | High


tileToString : Tile -> String
tileToString tile =
    case tile of
        Low ->
            "low"

        Medium ->
            "medium"

        High ->
            "high"
