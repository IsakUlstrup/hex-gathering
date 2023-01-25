module Content.Characters exposing (busStop, cactus, hibiscus, panda, sunflower)

import Character exposing (Character)
import HexEngine.Entity exposing (WorldPosition)


panda : Character
panda =
    Character '🐼' []


hibiscus : Character
hibiscus =
    Character '🌺' []


sunflower : Character
sunflower =
    Character '🌻' []


cactus : Character
cactus =
    Character '🌵' []


busStop : List WorldPosition -> Character
busStop destinations =
    Character '🚏'
        (List.map Character.Travel destinations)
