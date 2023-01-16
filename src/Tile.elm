module Tile exposing (Tile(..), tileToString)


type Tile
    = Water
    | Grass
    | Mountain


tileToString : Tile -> String
tileToString tile =
    case tile of
        Water ->
            "low"

        Grass ->
            "medium"

        Mountain ->
            "high"
