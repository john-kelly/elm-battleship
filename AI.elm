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
    unkownPositions = Grid.getUnknownPositions player.trackingGrid
    randomIntGen = Random.int 1 <| List.length unkownPositions
    (randomInt, nextSeed) = Random.generate randomIntGen (Random.initialSeed seed)
    shotPos =
      unkownPositions
        |> Array.fromList
        |> Array.get randomInt
  in
    case shotPos of
      Just pos ->
        Player.shoot pos player enemy
      Nothing ->
        (player, enemy)