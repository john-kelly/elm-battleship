module AI (shoot) where

-- Core
import Debug
import Array
import Random
-- 3rd party
import Matrix
-- Battleship
import Player
import Grid

randomShot : Int -> Player.Player -> Player.Player -> (Player.Player, Player.Player)
randomShot seed player enemy =
  let
    unkownPositions = Grid.getUnknownPositions player.trackingGrid
    randomIntGen = Random.int 0 ((List.length unkownPositions) - 1)
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

shoot : Int -> Player.Player -> Player.Player -> (Player.Player, Player.Player)
shoot seed player enemy =
  case Grid.nextShot player.trackingGrid of
    Just (x,y) ->
      Player.shoot (Debug.log "nextShot position:" (x,y)) player enemy
    Nothing ->
      randomShot (Debug.log "" seed) player enemy