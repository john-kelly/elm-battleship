module Fleet where

-- NOTE
-- serve as a wrapper around a list of ships so that we can give each added
-- ship a unique id. this may or may not be necessary.

-- Core
-- Evan
-- 3rd Party
-- Battleship
import Ship

type alias Fleet =
  { shipsSeen : Int
  , ships : List Ship.Ship
  }

init : List Ship.Ship -> Fleet
init ships =
  ships
    |> List.foldr addShip emptyFleet

emptyFleet : Fleet
emptyFleet =
  { shipsSeen = 0
  , ships = []
  }

defaultFleet : Fleet
defaultFleet =
  init
    [ Ship.init 2 Ship.Horizontal (0, 0)
    , Ship.init 3 Ship.Horizontal (0, 0)
    , Ship.init 4 Ship.Horizontal (0, 0)
    ]

addShip : Ship.Ship -> Fleet -> Fleet
addShip ship fleet =
  { fleet |
      ships <- {ship | id <- fleet.shipsSeen} :: fleet.ships,
      shipsSeen <- fleet.shipsSeen + 1
  }

map : (Ship.Ship -> Ship.Ship) -> Fleet -> Fleet
map fn fleet =
  { fleet |
      ships <- List.map fn fleet.ships
  }

toList : Fleet -> List Ship.Ship
toList fleet =
  fleet.ships
