module Content.Entities exposing (awesomesaurus, evergreen, mapTransition)

import Tile exposing (Entity(..))


evergreen : Entity
evergreen =
    Resource '🌲'


awesomesaurus : Entity
awesomesaurus =
    NPC '🦖'


mapTransition : String -> Entity
mapTransition destination =
    MapTransition destination
