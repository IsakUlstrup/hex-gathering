module Character exposing (Character, CharacterMsg(..))

import HexEngine.Entity exposing (WorldPosition)


type CharacterMsg
    = Travel WorldPosition


type alias Character =
    { icon : Char
    , actions : List CharacterMsg
    }
