module IslandMap exposing (mapInteraction)

import Dict
import Expect
import HexEngine.HexMap
import HexEngine.Point exposing (Point)
import Island exposing (Island, IslandMap)
import Test exposing (Test, test)


mapOne : ( Point, Island () )
mapOne =
    ( ( 0, 0, 0 ), Island.new "One" HexEngine.HexMap.empty )


mapTwo : ( Point, Island () )
mapTwo =
    ( ( 1, -1, 0 ), Island.new "Two" HexEngine.HexMap.empty )


mapThree : ( Point, Island () )
mapThree =
    ( ( 0, -1, 1 ), Island.new "Three" HexEngine.HexMap.empty )


exampleMap : IslandMap ()
exampleMap =
    Island.newMap mapOne [ mapTwo, mapThree ]


mapInteraction : Test
mapInteraction =
    Test.describe "Map Interaction"
        [ test "Select island two, confirm selected island is correct" <|
            \_ ->
                exampleMap
                    |> Island.selectMap (Tuple.first mapThree)
                    |> .selected
                    |> Tuple.second
                    |> .name
                    |> Expect.equal "Three"
        , test "Select island two, island one is moved back to all maps" <|
            \_ ->
                exampleMap
                    |> Island.selectMap (Tuple.first mapTwo)
                    |> .all
                    |> Expect.equal (Dict.fromList [ mapOne, mapThree ])
        , test "Select already selected island, map should be unchanged" <|
            \_ ->
                exampleMap
                    |> Island.selectMap (Tuple.first mapOne)
                    |> Expect.equal exampleMap
        ]
