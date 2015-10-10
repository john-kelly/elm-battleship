module Ship
  {-( Ship
  , init
  , getShipCoordinates
  , hasCoordinate
  )-} where

-- Core
-- Evan
-- 3rd Party
-- Battleship
import Location as Loc

type Orientation = Horizontal | Vertical

type alias Ship =
  { id : Int
  , length : Int
  , orientation : Orientation
  , location : Loc.Location
  , isSunk : Bool
  , added : Bool
  }

init : Int -> Orientation -> Loc.Location -> Ship
init length orientation location =
  { id = 0
  , length = length
  , orientation = orientation
  , location = location
  , isSunk = False
  , added = False
  }

setLocation : Loc.Location -> Ship -> Ship
setLocation newLocation ship =
  { ship | location <- newLocation }

setRow : Int -> Ship -> Ship
setRow row ship =
  { ship | location <- (row, (Loc.column ship.location)) }

getRow : Ship -> Int
getRow ship =
  Loc.row ship.location

setColumn : Int -> Ship -> Ship
setColumn column ship =
  { ship | location <- ((Loc.row ship.location), column) }

getColumn : Ship -> Int
getColumn ship =
  Loc.column ship.location

getShipCoordinates : Ship -> List Loc.Location
getShipCoordinates ship =
  let
    addToLocation =
      case ship.orientation of
        Vertical -> Loc.addToRow
        Horizontal -> Loc.addToColumn
  in
    List.repeat ship.length ship.location
      |> List.indexedMap addToLocation

toggleOrientation : Ship -> Ship
toggleOrientation ship =
  { ship |
    orientation <- if ship.orientation == Vertical then Horizontal else Vertical
  }

setAdded : Bool -> Ship -> Ship
setAdded bool ship =
  { ship | added <- bool }

hasCoordinate : Loc.Location -> Ship -> Bool
hasCoordinate coord ship =
  getShipCoordinates ship
    |> List.member coord

setAddedTrue : Ship -> Ship
setAddedTrue ship =
  setAdded True ship
