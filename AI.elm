module AI where

-- Core
import Array
import Random

-- Battleship
import Player
import Grid
import Ship

randomShot : Int -> Player.Player -> Player.Player -> (Player.Player, Player.Player)
randomShot seed player enemy =
  let
    getUnknownCoords = Grid.getUnknownCoords player.trackingGrid
    randomIntGen = Random.int 0 ((List.length getUnknownCoords) - 1)
    (randomInt, nextSeed) = Random.generate randomIntGen (Random.initialSeed seed)
    shotPos =
      getUnknownCoords
        |> Array.fromList
        |> Array.get randomInt
  in
    case shotPos of
      Just pos ->
        Player.shoot pos player enemy
      Nothing ->
        (player, enemy)
