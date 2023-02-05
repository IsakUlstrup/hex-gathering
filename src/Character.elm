module Character exposing (Character, CharacterData(..), CharacterInteraction(..))

import HexEngine.Entity exposing (WorldPosition)


type CharacterInteraction
    = Travel WorldPosition
    | IncrementCounter
    | DecrementCounter


type CharacterData
    = None
    | Counter Int
    | Description String


type alias Character =
    { icon : Char
    , interactions : List CharacterInteraction
    , data : CharacterData
    }
