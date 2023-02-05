module Character exposing (Character, CharacterData(..), CharacterInteraction(..), decrementCharacter, incrementCharacter)

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


incrementCharacter : Character -> Character
incrementCharacter char =
    case char.data of
        Counter c ->
            { char | data = Counter (c + 1) }

        _ ->
            char


decrementCharacter : Character -> Character
decrementCharacter char =
    case char.data of
        Counter c ->
            { char | data = Counter (c - 1) }

        _ ->
            char
