module Grid
  ( Grid
  , Context
  , toHtml
  , emptyPrimaryGrid
  , emptyTrackingGrid
  , isShipDestroyed
  , sinkShip
  , isShipSunk
  , addInvalidCoords
  , addShipCoords
  , shoot
  , setCoord
  , getHeight
  , getWidth
  , getUnknownCoords
  , coordsInBounds
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
import Location as Loc

(:=) = (,)

-- Grid
type alias IsHit = Bool
type alias Grid = Matrix.Matrix Cell
type alias Coord = Loc.Location
type Cell
  = Ship IsHit
  | Empty IsHit
  | Sunk
  | Unknown
  | Invalid

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

setCoord : Cell -> Coord -> Grid -> Grid
setCoord cell (row, col) grid =
  Matrix.set col row cell grid

setCoords : Cell -> List Coord -> Grid -> Grid
setCoords cell coords grid =
  coords
    |> List.foldr (setCoord cell) grid

addInvalidCoords : List Coord -> Grid -> Grid
addInvalidCoords coords grid =
  grid
    |> setCoords Invalid coords

addShipCoords : List Coord -> Grid -> Grid
addShipCoords coords grid =
  grid
    |> setCoords (Ship False) coords

coordsInBounds : List Coord -> Grid -> Bool
coordsInBounds coords grid =
  let
    gridH = getHeight grid
    gridW = getWidth grid
    isInBounds (shipRow, shipColumn) =
      shipRow >= 0 && shipRow < gridH && shipColumn >= 0 && shipColumn < gridW
  in
    coords
      |> List.map isInBounds
      |> List.all identity

isCoordCellType : Cell -> Coord -> Grid -> Bool
isCoordCellType cellType (row, col) grid =
  case Matrix.get col row grid of
    Just cell -> if cell == cellType then True else False
    Nothing -> False

isCellUnkown : Coord -> Grid -> Bool
isCellUnkown coord grid =
  isCoordCellType Unknown coord grid

isCellSunk : Coord -> Grid -> Bool
isCellSunk coord grid =
  isCoordCellType Sunk coord grid

isCellHit : Coord -> Grid -> Bool
isCellHit coord grid =
  isCoordCellType (Ship True) coord grid

getUnknownCoords : Grid -> List Coord
getUnknownCoords grid =
  grid
    |> Matrix.toIndexedArray
    |> Array.filter (snd >> ((==) Unknown))
    |> Array.map fst
    |> Array.toList
    |> List.map (\(col, row) -> (row, col))

isShipDestroyed : Grid -> Ship.Ship -> Bool
isShipDestroyed grid ship  =
  ship
    |> Ship.getShipCoordinates
    |> List.map (\coord -> isCellHit coord grid)
    |> List.all identity

sinkShip : Ship.Ship -> Grid -> Grid
sinkShip ship grid =
  ship
    |> Ship.getShipCoordinates
    |> List.foldr (\(row, col) g -> Matrix.set col row Sunk g) grid

isShipSunk : Ship.Ship -> Grid -> Bool
isShipSunk ship grid =
  let
    isCellSunk (row, col) grid =
      case Matrix.get col row grid of
        Just cell -> if cell == Sunk then True else False
        Nothing -> False
  in
  ship
    |> Ship.getShipCoordinates
    |> List.map (\coord -> isCellSunk coord grid)
    |> List.all identity

shoot : Coord -> Grid -> Cell
shoot (row, col) grid =
  case Matrix.get col row grid of
    Just cell ->
      case cell of
        Ship _ -> Ship True
        Empty _ -> Empty True
        Sunk -> cell
        Unknown -> cell
    Nothing -> -- Error
      Empty False

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
            Invalid -> events hc
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
        box "lightgray"
      else -- " "
        box "#99C2E1" -- Light blue
    Unknown -> -- "?"
      box "#F3F38B"
    Invalid ->
      box "#FF0000"
    Sunk ->
      box "black"

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
