module Character exposing (Character, CharacterMsg(..), new)

import HexEngine.Entity exposing (WorldPosition)


type CharacterMsg
    = Travel WorldPosition


type alias Character =
    { icon : Char
    , actions : List CharacterMsg
    }


new : Char -> List CharacterMsg -> Character
new icon actions =
    Character icon actions
