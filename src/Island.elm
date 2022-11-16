module Island exposing
    ( Island
    , IslandMap
    , Palette(..)
    , addEntity
    , new
    , newMap
    , selectMap
    )

import Dict exposing (Dict)
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)


type Palette
    = Pastel


type alias Entity e =
    { position : Point
    , model : e
    }


{-| An island is a hexgrid with some metadata
-}
type alias Island t e =
    { name : String
    , grid : HexMap t
    , palette : Palette
    , entities : List (Entity e)
    }


new : String -> HexMap t -> Island t e
new name grid =
    Island name grid Pastel []


addEntity : Point -> e -> Island t e -> Island t e
addEntity position model island =
    { island | entities = Entity position model :: island.entities }


{-| A hex grid containing islands
-}
type alias IslandMap t e =
    { selected : ( Point, Island t e )
    , all : Dict Point (Island t e)
    }


newMap : ( Point, Island t e ) -> List ( Point, Island t e ) -> IslandMap t e
newMap selected all =
    IslandMap selected (Dict.fromList all)


setSelected : Point -> Island t e -> IslandMap t e -> IslandMap t e
setSelected coordinate island map =
    { map
        | all =
            map.all
                |> Dict.insert (Tuple.first map.selected) (Tuple.second map.selected)
                |> Dict.remove coordinate
        , selected = ( coordinate, island )
    }


selectMap : Point -> IslandMap t e -> IslandMap t e
selectMap coordinate map =
    if Tuple.first map.selected == coordinate then
        map

    else
        case Dict.get coordinate map.all of
            Just i ->
                setSelected coordinate i map

            Nothing ->
                map
