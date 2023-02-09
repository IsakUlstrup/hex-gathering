module Character exposing (Character, CharacterState(..), decrementCharacter, incrementCharacter)

import HexEngine.Entity exposing (WorldPosition)



-- type CharacterInteraction
--     = Travel WorldPosition
--     | IncrementCounter
--     | DecrementCounter
--     | Display String


type alias GrowableData =
    { current : Int
    , max : Int
    }


type CharacterState
    = Counter Int
    | Description String
    | TravelDestination WorldPosition
    | Growable GrowableData


type alias Character =
    { icon : Char
    , states : List CharacterState
    }


updateCounter : (Int -> Int) -> CharacterState -> CharacterState
updateCounter f state =
    case state of
        Counter c ->
            Counter <| f c

        _ ->
            state


incrementCharacter : Character -> Character
incrementCharacter char =
    { char | states = List.map (updateCounter ((+) 1)) char.states }


decrementCharacter : Character -> Character
decrementCharacter char =
    { char | states = List.map (updateCounter (\x -> x - 1)) char.states }
