module Grid where

-- Core
-- Evan
import Html
import Html.Attributes
-- 3rd Party
import Matrix
import Matrix.Extra
-- Battleship
import Ship


-- Grid
type alias IsHit = Bool
type alias Grid = Matrix.Matrix Cell
type Cell
    = Ship IsHit
    | Empty IsHit
    | Unknown

cellToString : Cell -> String
cellToString cell =
  case cell of
    Ship isHit -> if isHit then "X" else "S"
    Empty isHit -> if isHit then "O" else " "
    Unknown -> "?"

defaultPrimaryGrid : Grid
defaultPrimaryGrid =
  Matrix.repeat 10 10 (Empty False)

defaultTrackingGrid : Grid
defaultTrackingGrid =
  Matrix.repeat 10 10 Unknown

getHeight : Grid -> Int
getHeight grid =
  Matrix.height grid

getWidth : Grid -> Int
getWidth grid =
  Matrix.width grid

addShip : Ship.Ship -> Grid -> Grid
addShip ship grid =
  let
  shipCoordinates = Ship.getShipCoordinates ship
  -- This is a bit tricky. Matrix.set takes col then row!!!
  setToShip (row, column) = Matrix.set column row (Ship False)
  in
  shipCoordinates
    |> List.foldr setToShip grid

toHtml : Grid -> Html.Html
toHtml grid =
  Html.div [ Html.Attributes.style [("display", "inline-block")] ]
  [ grid
      |> Matrix.map cellToString
      |> Matrix.Extra.prettyPrint
  ]
