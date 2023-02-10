module Character exposing (Character, CharacterState(..), decrementCharacter, grow, harvest, incrementCharacter)

import HexEngine.Entity exposing (WorldPosition)


type alias GrowableData =
    { current : Int
    , max : Int
    }


type alias Item =
    Char


type alias InventoryData =
    List ( Int, Item )


type CharacterState
    = Counter Int
    | Description String
    | TravelDestination WorldPosition
    | Growable GrowableData
    | Inventory InventoryData


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


updateGrowable : (GrowableData -> GrowableData) -> CharacterState -> CharacterState
updateGrowable f state =
    case state of
        Growable g ->
            Growable <| f g

        _ ->
            state



-- COUNTER


incrementCharacter : Character -> Character
incrementCharacter char =
    { char | states = List.map (updateCounter ((+) 1)) char.states }


decrementCharacter : Character -> Character
decrementCharacter char =
    { char | states = List.map (updateCounter (\x -> x - 1)) char.states }



-- GROWABLE


grow : Int -> Character -> Character
grow amount character =
    { character | states = List.map (updateGrowable (\g -> { g | current = min g.max (g.current + (amount // 4)) })) character.states }


harvest : Character -> Character
harvest character =
    { character | states = List.map (updateGrowable (\g -> { g | current = 0 })) character.states }
