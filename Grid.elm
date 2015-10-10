module Grid ( Grid, Context, toHtml, emptyPrimaryGrid, emptyTrackingGrid, canAddShip, addShip, showShip, hideShip, getHeight, getWidth ) where

-- Core
import Array -- For matrix conversion
-- Evan
import Html
import Html.Attributes
import Html.Events
-- 3rd Party
import Matrix
import Matrix.Extra
-- Battleship
import Ship
import Fleet

(=>) = (,)

-- Grid
type alias IsHit = Bool
type alias Grid = Matrix.Matrix Cell
type Cell
    = Ship IsHit
    | Empty IsHit
    -- TODO: Sunk state
    | Unknown

emptyPrimaryGrid : Grid
emptyPrimaryGrid =
  Matrix.repeat 10 10 (Empty False)

emptyTrackingGrid : Grid
emptyTrackingGrid =
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
    List.foldr setToShip grid shipCoordinates

showShip : Ship.Ship -> Fleet.Fleet -> Grid -> Grid
showShip ship fleet grid =
  let
    shipCoordinates = Ship.getShipCoordinates ship
    transform x y cell =
      if List.member (y,x) shipCoordinates then
        (Ship False)
      else
        cell
  in
    if canAddShip ship fleet grid then
      Matrix.indexedMap transform grid
    else
      grid

hideShip : Ship.Ship -> Fleet.Fleet -> Grid -> Grid
hideShip ship fleet grid =
  let
    shipCoordinates = Ship.getShipCoordinates ship
    addedShipsCoords = fleet
      |> Fleet.toList
      |> List.filter .added
      |> List.map Ship.getShipCoordinates
      |> List.concat
    condition x y =
      List.member (y,x) shipCoordinates
    condition2 x y =
      not <| List.member (y,x) addedShipsCoords
    transform x y cell =
      if (condition x y) && (condition2 x y) then
        (Empty False)
      else
        cell
  in
    Matrix.indexedMap transform grid

canAddShip : Ship.Ship -> Fleet.Fleet -> Grid -> Bool
canAddShip ship fleet grid =
  -- order here is important for optimization. `shipInBounds` is cheap
  if | not (shipInBounds ship grid) -> False
     | shipOverlaps ship fleet -> False
     | otherwise -> True

-- private helper for canAddShip
shipOverlaps : Ship.Ship -> Fleet.Fleet -> Bool
shipOverlaps ship fleet =
  let
  shipCoordinates = Ship.getShipCoordinates ship
  in
  fleet
    |> Fleet.toList
    |> List.filter .added
    |> List.map Ship.getShipCoordinates
    |> List.concat
    |> List.foldr (\coord acc -> (List.member coord shipCoordinates) || acc) False

-- private helper for canAddShip
shipInBounds : Ship.Ship -> Grid -> Bool
shipInBounds ship grid =
  let
  gridH = getHeight grid
  gridW = getWidth grid
  isInBounds (shipRow, shipColumn) =
    shipRow >= 0 && shipRow < gridH && shipColumn >= 0 && shipColumn < gridW
  in
  ship
    |> Ship.getShipCoordinates
    |> List.map isInBounds
    |> List.all identity

type alias Context =
  { hover : Signal.Address (Maybe (Int, Int))
  , click : Signal.Address ()
  }

cellToHtml : Maybe Context -> Int -> Int -> Cell -> Html.Html
cellToHtml hoverClick y x cell =
  let
    pos = (x, y)
    style =
      [ ("height", "40px")
      , ("width", "40px")
      , ("border-radius", "5px")
      , ("margin", "1px")
      ]
    adm =
      case hoverClick of
        Just hc ->
          [ Html.Events.onMouseEnter hc.hover (Just pos)
          , Html.Events.onClick hc.click ()
          ]
        Nothing ->
          []
    box color = Html.div
      ([ Html.Attributes.style <| ("background-color", color) :: style
       , Html.Attributes.class "cell"
       ] ++ adm) []
  in
  case cell of
    Ship isHit ->
      if isHit then -- "X"
        box "#F60018" -- Red
      else -- "S"
        box "#808080" -- Gray
    Empty isHit ->
      if isHit then -- "O"
        box "white"
      else -- " "
        box "#99C2E1" -- Light blue
    Unknown -> -- "?"
      box "yellow"

toHtmlRows : Matrix.Matrix Html.Html -> List Html.Html
toHtmlRows matrixHtml =
  let
    rowNumbers = [0..(Matrix.height matrixHtml)-1]
    maybeArrayToList : Maybe (Array.Array a) -> List a
    maybeArrayToList array =
      case array of
        Just ary -> Array.toList ary
        Nothing -> []
    transform rowNum list =
      (Html.div
        [ Html.Attributes.style
          [ "display" => "flex" ]
        ] <| maybeArrayToList <| Matrix.getRow rowNum matrixHtml) :: list
  in
    List.foldr transform [] rowNumbers

toHtml : Maybe Context -> Grid -> Html.Html
toHtml context grid =
  let
    event =
      case context of
        Just adm ->
          [Html.Events.onMouseLeave adm.hover Nothing]
        Nothing ->
          []
  in
  Html.div
  ([ Html.Attributes.class "battlefield"
    , Html.Attributes.style []
    ] ++ event)
  (grid
    |> Matrix.indexedMap (cellToHtml context)
    |> toHtmlRows)