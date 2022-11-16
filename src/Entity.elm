module Entity exposing (Entity(..))

import Entities.Counter
import Entities.Timer


type Entity
    = Counter Entities.Counter.Model
    | Timer Entities.Timer.Model
