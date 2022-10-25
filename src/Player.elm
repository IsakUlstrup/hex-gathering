module Player exposing (Player, playerCooldown, playerMove, playerpath)

import HexEngine.Point as Point exposing (Point)


type alias Player =
    { position : Point
    , icon : Char
    , path : Maybe (List Point)
    , moveCooldown : Int
    }


playerpath : (Point -> Bool) -> Point -> Player -> Player
playerpath walkable to player =
    { player | path = Point.pathfind walkable player.position to }


playerCooldown : Int -> Player -> Player
playerCooldown dt player =
    { player | moveCooldown = max 0 (player.moveCooldown - dt) }


playerMove : Player -> Player
playerMove player =
    case player.path of
        Just (t :: ts) ->
            if player.moveCooldown <= 0 then
                { player
                    | position = t
                    , path = Just ts
                    , moveCooldown = 800
                }

            else
                player

        _ ->
            player
