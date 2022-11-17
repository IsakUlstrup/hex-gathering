module Island exposing
    ( Island
    , Palette(..)
    , addEntity
    , getPoint
    , mapEntities
    , new
    , updateEntity
    )

import Dict exposing (Dict)
import HexEngine.HexMap exposing (HexMap)
import HexEngine.Point exposing (Point)


type Palette
    = Pastel


{-| An island is a hexgrid with some metadata
-}
type alias Island t e =
    { name : String
    , grid : HexMap t
    , palette : Palette
    , entities : Dict Point e
    }


new : String -> HexMap t -> Island t e
new name grid =
    Island name grid Pastel Dict.empty


addEntity : Point -> e -> Island t e -> Island t e
addEntity position model island =
    { island | entities = Dict.insert position model island.entities }


getPoint : Point -> Island t e -> ( Maybe e, Maybe t )
getPoint position island =
    ( Dict.get position island.entities, Dict.get position island.grid )


updateEntity : Point -> (e -> e) -> Island t e -> Island t e
updateEntity position f island =
    { island
        | entities =
            Dict.update position
                (\m ->
                    case m of
                        Just e ->
                            Just <| f e

                        Nothing ->
                            m
                )
                island.entities
    }


mapEntities : (e -> e) -> Island t e -> Island t e
mapEntities f island =
    { island | entities = Dict.map (\_ v -> f v) island.entities }
