module Player (Player, defaultPlayer, defaultComputer, toHtml, allShipsAdded, getShips, updateShip, turnShip, moveShip, updateGrid, nextNotAddedShipId, addShip) where

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
  let
    newShip = Fleet.updateShip shipId fn player.fleet
  in
    { player | fleet <- newShip }

turnShip : Int -> Maybe (Int, Int) -> Player -> Player
turnShip shipId pos player =
  let
    -- Find current ship by id
    ship =
      case Fleet.getShip shipId player.fleet of
        Just ship ->
          ship
        Nothing -> -- Never gonna happen
          Ship.init 1 Ship.Horizontal (0,0)
    -- Erase current ship from the grid
    grid = Grid.hideShip ship player.fleet player.primaryGrid
    nextShip = Ship.toggleOrientation ship
    nextGrid =
      case pos of
        Just _ ->
          Grid.showShip nextShip player.fleet grid
        Nothing ->
          grid
  in
    updateGrid nextGrid player
      |> updateShip shipId (\_ -> nextShip) 

-- Repositions ship on the grid:
  -- 1. Erase the current one
  -- 2. Draw the new one (if given position)
moveShip : Int -> Maybe (Int, Int) -> Player -> Player
moveShip shipId position player =
  let
    -- Find current ship by id
    ship = -- TODO: Get rid of this mess
      case Fleet.getShip shipId player.fleet of
        Just ship ->
          ship
        Nothing -> -- Never gonna happen
          Ship.init 1 Ship.Horizontal (0,0)
    -- Erase the current ship
    grid = Grid.hideShip ship player.fleet player.primaryGrid
  in
    case position of
      Just pos -> -- New position
        let
          nextShip =
            case Fleet.getShip shipId player.fleet of
              Just ship ->
                Ship.setLocation pos ship
              Nothing -> -- Never gonna happen
                Ship.init 1 Ship.Horizontal (0,0)
          -- Put the next ship on the grid
          newGrid = Grid.showShip nextShip player.fleet grid
          fn s = Ship.setLocation pos s
        in
        updateGrid newGrid <| updateShip shipId fn player
      Nothing -> -- Old position (hide ship)
        updateGrid grid player

updateGrid : Grid.Grid -> Player -> Player
updateGrid grid player =
  { player | primaryGrid <- grid }

getShips : Player -> List Ship.Ship
getShips player =
  player.fleet
    |> Fleet.toList

nextNotAddedShipId : Player -> Maybe Int
nextNotAddedShipId player =
  let
    ship = (getShips player)
      |> List.filter (not << .added)
      |> List.head
  in
    case ship of
      Just s -> Just s.id
      Nothing -> Nothing

toHtml : Maybe Grid.Context -> Player -> Html.Html
toHtml address player =
  Html.div []
  [ Grid.toHtml address player.primaryGrid
  --, Grid.toHtml player.trackingGrid
  ]