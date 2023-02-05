module Content.Characters exposing (busStop, cactus, counter, hibiscus, panda, sunflower)

import Character exposing (Character, CharacterData(..), CharacterInteraction(..))
import HexEngine.Entity exposing (WorldPosition)


panda : Character
panda =
    Character '🐼' [] None


hibiscus : Character
hibiscus =
    Character '🌺' [] (Description "A pretty flower")


sunflower : Character
sunflower =
    Character '🌻' [] None


cactus : Character
cactus =
    Character '🌵' [] None


counter : Character
counter =
    Character '🧮' [ IncrementCounter, DecrementCounter ] (Counter 0)


busStop : List WorldPosition -> Character
busStop destinations =
    Character '🚏'
        (List.map Character.Travel destinations)
        None
