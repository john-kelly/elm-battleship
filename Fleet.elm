module Fleet where

-- NOTE
-- serve as a wrapper around a Dict of ships so that we can give each added
-- ship a unique id.

-- Core
import Dict
-- Evan
-- 3rd Party
-- Battleship
import Ship

type alias Fleet =
  { shipsSeen : Int
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
