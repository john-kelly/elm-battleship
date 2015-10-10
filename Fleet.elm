module Fleet (Fleet, shipSizes, defaultFleet, random, getShip, updateShip, toList) where

-- NOTE
-- serve as a wrapper around a Dict of ships so that we can give each added
-- ship a unique id.

-- Core
import Dict
import Random
-- Evan
-- 3rd Party
-- Battleship
import Ship

type alias Fleet =
  { shipsSeen : Int -- Is that really needed?
  , ships : Dict.Dict Int Ship.Ship
  }

init : List Ship.Ship -> Fleet
init ships =
  ships
    |> List.foldr addShip emptyFleet

emptyFleet : Fleet
emptyFleet =
  { shipsSeen = 0
  , ships = Dict.empty
  }

shipSizes = [2..5]

shipsWithSizes = List.map Ship.init shipSizes

defaultFleet : Fleet
defaultFleet =
  init <|
    List.map (\fn -> fn Ship.Horizontal (0, 0)) shipsWithSizes

random : Int -> Fleet
random seed =
  let
    locationsGen =
      Random.list (List.length shipSizes)
        <| Random.pair (Random.int 0 10) (Random.int 0 10)
    floatsGen =
      Random.list (List.length shipSizes)
        <| Random.float 0 1
    (locations, seed1) =
      Random.generate locationsGen <| Random.initialSeed seed
    (floats, seed2) =
    -- ? Not sure whether we have to use a different seed here:
      Random.generate floatsGen <| Random.initialSeed seed
    floatToOrientation float =
      if float < 0.5 then Ship.Horizontal else Ship.Vertical
    orientations = List.map floatToOrientation floats
  in
    init <|
      List.map3 (\fn orient loc -> fn orient loc) shipsWithSizes orientations locations

addShip : Ship.Ship -> Fleet -> Fleet
addShip ship fleet =
  { fleet |
      ships <- fleet.ships
        |> Dict.insert fleet.shipsSeen {ship | id <- fleet.shipsSeen},
      shipsSeen <- fleet.shipsSeen + 1
  }

getShip : Int -> Fleet -> Maybe Ship.Ship
getShip shipId fleet =
  Dict.get shipId fleet.ships

map : (Ship.Ship -> Ship.Ship) -> Fleet -> Fleet
map fn fleet =
  { fleet |
      ships <- fleet.ships
        |> Dict.map (\compareable ship -> fn ship)
  }

toList : Fleet -> List Ship.Ship
toList fleet =
  fleet.ships
    |> Dict.values

updateShip : Int -> (Ship.Ship -> Ship.Ship) -> Fleet -> Fleet
updateShip shipId fn fleet =
  { fleet |
      ships <- fleet.ships
        |> Dict.update shipId (Maybe.map fn)
  }
