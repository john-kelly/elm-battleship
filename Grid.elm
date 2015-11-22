module Grid
  ( Grid
  , Cell
  , Context
  , toHtml
  , emptyPrimaryGrid
  , emptyTrackingGrid
  , addShip
  , isShipDestroyed
  , sinkShip
  , isShipSunk
  , addInvalidShip
  , shoot
  , setCell
  , getHeight
  , getWidth
  , getUnknownPositions
  , nextShot
  , shipInBounds
  ) where

-- Core
import Array -- For matrix conversion
import Debug
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

addShip : Ship.Ship -> Grid -> Grid
addShip ship grid =
  ship
    |> Ship.getShipCoordinates
    |> List.foldr (\(row, column) -> Matrix.set column row (Ship False)) grid

addInvalidShip : Ship.Ship -> Grid -> Grid
addInvalidShip ship grid =
  ship
    |> Ship.getShipCoordinates
    |> List.foldr (\(row, column) -> Matrix.set column row Invalid) grid

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


setCell : Loc.Location -> Cell -> Grid -> Grid
setCell (row, col) cell grid =
  Matrix.set col row cell grid

-- AI helper
getUnknownPositions : Grid -> List (Int, Int)
getUnknownPositions grid =
  grid
    |> Matrix.toIndexedArray
    |> Array.filter (snd >> ((==) Unknown))
    |> Array.map fst
    |> Array.toList
    |> List.map (\(y,x) -> (x,y))

-- AI helper
nextShot : Grid -> Maybe Loc.Location
nextShot grid =
  let
    hitCellLoc =
      grid
        |> Matrix.toIndexedArray
        |> Array.foldl (\((x,y), cell) hitLoc ->
          if hitLoc == Nothing && cell == (Ship True) then
            Just (x,y) else hitLoc)
        Nothing
    isEmptyHit (x,y) =
      case Matrix.get x y grid of
        Just cell ->
          cell == (Empty True)
        Nothing ->
          False
    isUnkown (x,y) =
      case Matrix.get x y grid of
        Just cell ->
          cell == Unknown
        Nothing ->
          False
    isHit (x,y) =
      case Matrix.get x y grid of
        Just cell ->
          cell == (Ship True)
        Nothing ->
          False
    findUnknown (x,y) =
      let
        right = (x+1,y)
        bottom = (x,y+1)
        left = (x-1,y)
        top = (x,y-1)
      in
        if isHit right && isEmptyHit left then findUnknown right
           else if isHit right && isHit left then findUnknown right
           else if isHit right then left -- FIXME
           else if isHit bottom && isEmptyHit top then findUnknown bottom
           else if isHit bottom && isHit top then findUnknown bottom
           else if isHit bottom then top -- FIXME
           else if isUnkown left then left
           else if isUnkown right then right
           else if isUnkown bottom then bottom
           else if isUnkown top then top
           else bottom
  in
    case hitCellLoc of
      Just loc ->
        Just <| (\(a,b) -> (b,a)) <| findUnknown loc
      Nothing ->
        Nothing

shoot : Loc.Location -> Grid -> Cell
shoot (row, col) grid =
  case Matrix.get col row grid of
    Just cell ->
      case cell of
        Ship _ -> Ship True
        Empty _ -> Empty True
        _ -> cell -- Is still this ok to do?
    Nothing -> -- Error
      Empty False

isShipDestroyed : Grid -> Ship.Ship -> Bool
isShipDestroyed grid ship  =
  ship
    |> Ship.getShipCoordinates
    |> List.map (\coord -> isCellHit coord grid)
    |> List.all identity

isCellHit : Loc.Location -> Grid -> Bool
isCellHit (row, col) grid =
  case Matrix.get col row grid of
    Just cell -> if cell == (Ship True) then True else False
    Nothing -> False

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
       ] ++ adm) [{-Html.text (toString pos)-}]
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
   , Html.Attributes.style ["display" := "inline-block"]
   ] ++ event)
  (grid
    |> Matrix.indexedMap (cellToHtml context)
    |> toHtmlRows)
