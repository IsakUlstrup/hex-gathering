module Entity exposing (Entity(..))

import Entities.Counter
import Entities.Timer
import Player exposing (Player)


type Entity
    = Counter Entities.Counter.Model
    | Timer Entities.Timer.Model
    | Player Player
