module Character exposing (Character, CharacterData(..), CharacterInteraction(..), decrementCharacter, incrementCharacter)

import HexEngine.Entity exposing (WorldPosition)


type CharacterInteraction
    = Travel WorldPosition
    | IncrementCounter
    | DecrementCounter
    | Display String


type CharacterData
    = None
    | Counter Int
    | Description String


type alias Character =
    { icon : Char
    , interactions : List CharacterInteraction
    , state : CharacterData
    }


incrementCharacter : Character -> Character
incrementCharacter char =
    case char.state of
        Counter c ->
            { char | state = Counter (c + 1) }

        _ ->
            char


decrementCharacter : Character -> Character
decrementCharacter char =
    case char.state of
        Counter c ->
            { char | state = Counter (c - 1) }

        _ ->
            char
