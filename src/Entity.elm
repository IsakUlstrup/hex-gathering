module Entity exposing (Entity(..), counterUpdate, timerUpdate)

import Entities.Counter
import Entities.Timer
import Player exposing (Player)


type Entity
    = Counter Entities.Counter.Model
    | Timer Entities.Timer.Model
    | Player Player


counterUpdate : Entities.Counter.Msg -> Entity -> Entity
counterUpdate msg entity =
    case entity of
        Counter model ->
            Counter <| Entities.Counter.update msg model

        _ ->
            entity


timerUpdate : Entities.Timer.Msg -> Entity -> Entity
timerUpdate msg entity =
    case entity of
        Timer model ->
            Timer <| Entities.Timer.update msg model

        _ ->
            entity
