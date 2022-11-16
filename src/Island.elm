module Island exposing
    ( Island
    , IslandMap
    , Palette(..)
    , new
    , newMap
    , selectMap
    )

import Dict exposing (Dict)
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)


type Palette
    = Pastel


{-| An island is a hexgrid with some metadata
-}
type alias Island t =
    { name : String
    , grid : HexMap t
    , palette : Palette
    }


new : String -> HexMap t -> Island t
new name grid =
    Island name grid Pastel


{-| A hex grid containing islands
-}
type alias IslandMap t =
    { selected : ( Point, Island t )
    , all : Dict Point (Island t)
    }


newMap : ( Point, Island t ) -> List ( Point, Island t ) -> IslandMap t
newMap selected all =
    IslandMap selected (Dict.fromList all)


setSelected : Point -> Island t -> IslandMap t -> IslandMap t
setSelected coordinate island map =
    { map
        | all =
            map.all
                |> Dict.insert (Tuple.first map.selected) (Tuple.second map.selected)
                |> Dict.remove coordinate
        , selected = ( coordinate, island )
    }


selectMap : Point -> IslandMap t -> IslandMap t
selectMap coordinate map =
    if Tuple.first map.selected == coordinate then
        map

    else
        case Dict.get coordinate map.all of
            Just i ->
                setSelected coordinate i map

            Nothing ->
                map
