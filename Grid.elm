module Grid
  ( Grid
  , Context
  , toHtml
  , emptyPrimaryGrid
  , emptyTrackingGrid
  , canAddShip
  , addShip
  , showShip
  , hideShip
  , shoot
  , setCell
  , getHeight
  , getWidth
  ) where

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

(:=) = (,)

-- Grid
type alias IsHit = Bool
type alias Grid = Matrix.Matrix Cell
type Cell
    = Ship IsHit
    | Empty IsHit
    | Sunk
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

setShip : Cell -> Ship.Ship -> Fleet.Fleet -> Grid -> Grid
setShip newCell ship fleet grid =
  let
    transform col row oldCell =
      if Ship.hasCoordinate (row, col) ship then newCell else oldCell
  in
    if canAddShip ship fleet grid then
      grid
        |> Matrix.indexedMap transform
    else
      grid

showShip : Ship.Ship -> Fleet.Fleet -> Grid -> Grid
showShip ship fleet grid =
  setShip (Ship False) ship fleet grid

hideShip : Ship.Ship -> Fleet.Fleet -> Grid -> Grid
hideShip ship fleet grid =
  setShip (Empty False) ship fleet grid

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


setCell : (Int, Int) -> Cell -> Grid -> Grid
setCell (j, i) cell grid =
  Matrix.set i j cell grid

shoot : (Int, Int) -> Grid -> Cell
shoot (j, i) grid =
  case Matrix.get i j grid of
    Just cell ->
      case cell of
        Ship _ ->
          Ship True
        Empty _ ->
          Empty True
        Sunk ->
          Sunk
        Unknown ->
          Unknown
    Nothing -> -- Error
      Empty False

isShipSunk : Ship.Ship -> Grid -> Bool
isShipSunk ship grid =
  let
    isHit (row, column) =
      case Matrix.get column row grid of
        Just cell ->
          if cell == (Ship True) then True else False
        Nothing ->
          False
  in
    ship
      |> Ship.getShipCoordinates
      |> List.map isHit
      |> List.foldr (&&) True

type alias Context =
  { hover : Signal.Address (Maybe (Int, Int))
  , click : Signal.Address (Int, Int)
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
    events hc =
      [ Html.Events.onMouseEnter hc.hover (Just pos)
      , Html.Events.onMouseDown hc.click pos
      ]
    adm =
      case hoverClick of
        Just hc ->
          case cell of
            Ship False -> events hc
            Empty False -> events hc
            Unknown -> events hc
            Ship True -> []
            Empty True -> []
            Sunk -> []
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
      box "#F3F38B"

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
          [ "display" := "flex" ]
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
