module Content.Entities exposing (awesomesaurus, evergreen, mapTransition)

import HexEngine.Point exposing (Point)
import Tile exposing (Entity(..))


evergreen : Entity
evergreen =
    Resource '🌲'


awesomesaurus : Entity
awesomesaurus =
    NPC '🦖'


mapTransition : Point -> Entity
mapTransition destination =
    MapTransition destination
