module Content.Characters exposing (busStop, cactus, counter, hibiscus, panda, sunflower)

import Character exposing (Character, CharacterState(..))
import HexEngine.Entity exposing (WorldPosition)


panda : Character
panda =
    Character '🐼' []


hibiscus : Character
hibiscus =
    Character '🌺'
        [ Description "Hibiscus"
        , Growable { current = 50, max = 1000 }
        ]


sunflower : Character
sunflower =
    Character '🌻' []


cactus : Character
cactus =
    Character '🌵' []


counter : Character
counter =
    Character '🧮'
        [ Counter 0
        , Description "A counter"
        ]


busStop : List WorldPosition -> Character
busStop destinations =
    Character '🚏'
        (List.map TravelDestination destinations)
