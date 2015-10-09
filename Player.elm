module Player (Player, defaultPlayer, defaultComputer, toHtml, allShipsAdded, getShips, updateShip, updateGrid, addShip) where

-- The player manages the syn b/w the ships in a fleet and the grid.
-- There is an implicit invariant b/w a ship a fleet and a grid which is that if
-- a ship is `added` then it means that its information has been added to the
-- grid.

-- Core
-- Evan
import Html
-- 3rd Party
import Matrix
-- Battleship
import Fleet
import Grid
import Ship

-- Player
type alias Player =
    { fleet : Fleet.Fleet
    , primaryGrid : Grid.Grid
    , trackingGrid : Grid.Grid
    }
defaultPlayer : Player
defaultPlayer =
    { fleet = Fleet.defaultFleet
    , primaryGrid = Grid.defaultPrimaryGrid
    , trackingGrid = Grid.defaultTrackingGrid
    }
-- TODO Setup a random board for the computer.
-- This will be different than the defaultPlayer function.
defaultComputer : Player
defaultComputer =
    { fleet = Fleet.defaultFleet
    , primaryGrid = Grid.defaultPrimaryGrid
    , trackingGrid = Grid.defaultTrackingGrid
    }

addShip : Int -> Player -> Player
addShip shipId player =
  case Fleet.getShip shipId player.fleet of
    Just ship ->
      if Grid.canAddShip ship player.fleet player.primaryGrid then
        { player |
            fleet <- Fleet.updateShip shipId Ship.setAddedTrue player.fleet,
            primaryGrid <- Grid.addShip ship player.primaryGrid
        }
      else
        player
    Nothing -> player

{-
showShip : Ship.Ship -> Fleet.Fleet -> Grid.Grid -> Grid.Grid
showShip ship fleet grid =
  if canAddShip ship fleet grid then
    Grid.showShip ship grid
  else
    grid
-}
--hideShip : Ship.Ship -> Fleet.Fleet -> Grid.Grid -> Grid.Grid
--hideShip ship fleet grid =
--  Grid.hideShip ship grid

allShipsAdded : Player -> Bool
allShipsAdded player =
  player
    |> getShips
    |> List.map .added
    |> List.all identity

updateShip : Int -> (Ship.Ship -> Ship.Ship) -> Player -> Player
updateShip shipId fn player =
  { player | fleet <- Fleet.updateShip shipId fn player.fleet }

updateGrid : Grid.Grid -> Player -> Player
updateGrid grid player =
  { player | primaryGrid <- grid }

getShips : Player -> List Ship.Ship
getShips player =
  player.fleet
    |> Fleet.toList


toHtml : Maybe Grid.Context -> Player -> Html.Html
toHtml address player =
  Html.div []
  [ Grid.toHtml address player.primaryGrid
  --, Grid.toHtml player.trackingGrid
  ]