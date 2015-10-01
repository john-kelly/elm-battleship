module Ship where

-- Core
-- Evan
-- 3rd Party
-- Battleship
import Location as Loc

type Orientation = Horizontal | Vertical

type alias Ship =
  { length : Int
  , orientation : Orientation
  , location : Loc.Location
  }

init : Int -> Orientation -> Loc.Location -> Ship
init length orientation location =
  { length = length
  , orientation = orientation
  , location = location
  }

getShipCoordinates : Ship -> List Loc.Location
getShipCoordinates ship =
  let
    lengthRange = [0 .. ship.length - 1]
    locationRepeat = List.repeat ship.length ship.location
  in
    case ship.orientation of
      Vertical ->
        List.map2 Loc.addToRow lengthRange locationRepeat
      Horizontal ->
        List.map2 Loc.addToColumn lengthRange locationRepeat
