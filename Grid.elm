module Grid ( Grid, toHtml , defaultPrimaryGrid , defaultTrackingGrid , addShip , getHeight , getWidth ) where

-- Core
import Array -- For matrix conversion
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
    -- TODO: Sunk state
    | Unknown

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

cellToHtml : Cell -> Html.Html
cellToHtml cell =
  let
    style =
      [ ("display", "inline-block")
      , ("height", "40px")
      , ("width", "40px")
      , ("border-radius", "5px")
      , ("vertical-align", "top") -- Fix horizontal spaces
      , ("margin", "1px")
      ]
    box color = Html.div
      [ Html.Attributes.style <| ("background-color", color) :: style
      ] []
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
      (Html.div [] <| maybeArrayToList <| Matrix.getRow rowNum matrixHtml) :: list
  in
    List.foldr transform [] rowNumbers

toHtml : Grid -> Html.Html
toHtml grid =
  Html.div [Html.Attributes.class "battlefield"]
  (grid
    |> Matrix.map cellToHtml
    |> toHtmlRows)