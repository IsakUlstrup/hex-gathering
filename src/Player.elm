module Player exposing
    ( Player
    , moveStateString
    , moveTarget
    , new
    , playerCooldown
    , playerMove
    , playerpath
    )

import HexEngine.Point as Point exposing (Point)


type MoveState
    = Moving (List Point) Int
    | Cooling (List Point) Int
    | Idle


type alias Player =
    { position : Point
    , icon : Char
    , moveState : MoveState
    }


moveTime : Int
moveTime =
    800


new : Point -> Char -> Player
new position icon =
    Player position icon Idle


moveStateString : Player -> String
moveStateString player =
    case player.moveState of
        Moving _ _ ->
            "moving"

        Cooling _ _ ->
            "cooling"

        Idle ->
            "idle"


setPlayerPath : List Point -> Player -> Player
setPlayerPath path player =
    case player.moveState of
        Moving _ cd ->
            { player | moveState = Moving path cd }

        Cooling _ cd ->
            { player | moveState = Cooling path cd }

        Idle ->
            { player | moveState = Cooling path 0 }


playerpath : (Point -> Bool) -> Point -> Player -> Player
playerpath walkable to player =
    case moveTarget player of
        Just p ->
            if p == to then
                { player | moveState = Idle }

            else
                case Point.pathfind walkable player.position to of
                    Just path ->
                        setPlayerPath path player

                    --{ player | moveState = Cooling path 0 }
                    Nothing ->
                        { player | moveState = Idle }

        Nothing ->
            case Point.pathfind walkable player.position to of
                Just path ->
                    setPlayerPath path player

                --{ player | moveState = Cooling path 0 }
                Nothing ->
                    { player | moveState = Idle }


playerCooldown : Int -> Player -> Player
playerCooldown dt player =
    case player.moveState of
        Moving path cd ->
            { player | moveState = Moving path (max 0 (cd - dt)) }

        Cooling path cd ->
            { player | moveState = Cooling path (max 0 (cd - dt)) }

        Idle ->
            player


moveTarget : Player -> Maybe Point
moveTarget player =
    case player.moveState of
        Moving path _ ->
            path |> List.reverse |> List.head

        Cooling path _ ->
            path |> List.reverse |> List.head

        Idle ->
            Nothing


playerMove : Player -> Player
playerMove player =
    case player.moveState of
        Moving path cd ->
            if cd == 0 then
                { player | moveState = Cooling path 300 }

            else
                player

        Cooling (p :: path) cd ->
            if cd <= 0 then
                { player
                    | moveState = Moving path moveTime
                    , position = p
                }

            else
                player

        Cooling [] cd ->
            if cd <= 0 then
                { player
                    | moveState = Idle
                }

            else
                player

        Idle ->
            player



-- Just (t :: ts) ->
--     if player.moveCooldown <= 0 then
--         { player
--             | position = t
--             , path = Just ts
--             , moveCooldown = moveTime
--             , moveState = Moving
--         }
--     else
--         player
-- _ ->
--     player
