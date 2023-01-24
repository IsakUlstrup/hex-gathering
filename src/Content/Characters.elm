module Content.Characters exposing (airplane, cactus, hibiscus, panda, sunflower)

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


airplane : WorldPosition -> Character
airplane destination =
    Character '🛫' [ Character.Travel destination ]
