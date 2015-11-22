module Player
  ( Player
  , defaultPlayer
  , defaultComputer
  , random
  , allShipsAdded
  , getShips
  , updateShip
  , updateGrid
  , nextNotAddedShipId
  , addShip
  , shoot
  , viewPrimaryGrid
  , viewTrackingGrid
  , previewShip
  , canAddShip
  , allShipsSunk
  ) where

-- The player manages the syn b/w the ships in a fleet and the grid.
-- There is an implicit invariant b/w a ship a fleet and a grid which is that if
-- a ship is `added` then it means that its information has been added to the
-- grid.

-- Core
-- Evan
import Html
-- 3rd Party
-- Battleship
import Fleet
import Grid
import Location as Loc
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
    , primaryGrid = Grid.emptyPrimaryGrid
    , trackingGrid = Grid.emptyTrackingGrid
    }
-- TODO Setup a random board for the computer.
-- This will be different than the defaultPlayer function.
defaultComputer : Player
defaultComputer =
    { fleet = Fleet.defaultFleet
    , primaryGrid = Grid.emptyPrimaryGrid
    , trackingGrid = Grid.emptyTrackingGrid
    }

random : Int -> Player
random seed =
  let
    player = Player (Fleet.random seed) Grid.emptyPrimaryGrid Grid.emptyTrackingGrid
    shipIDs = List.indexedMap (\i _ -> i) Fleet.shipSizes
    newPlayer = List.foldr addShip player shipIDs
  in
    -- Is it okay to just increment a seed to get a new one?
    if allShipsAdded newPlayer then newPlayer else random <| seed + 1

addShip : Int -> Player -> Player
addShip shipId player =
  case Fleet.getShip shipId player.fleet of
    Just ship ->
      if canAddShip ship player then
        { player |
            -- This is important here. Both the ship in the fleet and the grid
            -- are updated when a ship is added.
            fleet = Fleet.updateShip shipId Ship.setAddedTrue player.fleet,
            primaryGrid = Grid.addShip ship player.primaryGrid
        }
      else
        player
    Nothing -> player

allShipsAdded : Player -> Bool
allShipsAdded player =
  player
    |> getShips
    |> List.map .added
    |> List.all identity

-- Win/lose checker
allShipsSunk : Player -> Bool
allShipsSunk player =
  player
    |> getShips
    |> List.map (\ship -> Grid.isShipSunk ship player.primaryGrid)
    |> List.all identity

updateShip : Int -> (Ship.Ship -> Ship.Ship) -> Player -> Player
updateShip shipId fn player =
  let
    newShip = Fleet.updateShip shipId fn player.fleet
  in
    { player | fleet = newShip }

canAddShip : Ship.Ship -> Player -> Bool
canAddShip ship player =
  -- order here is important for optimization. `shipInBounds` is cheap
  if not (Grid.shipInBounds ship player.primaryGrid) then
    False
     else if Fleet.shipOverlaps ship player.fleet then
      False
     else True

updateGrid : Grid.Grid -> Player -> Player
updateGrid grid player =
  { player | primaryGrid = grid }

getShips : Player -> List Ship.Ship
getShips player =
  player.fleet
    |> Fleet.toList

getShip : Int -> Player -> Maybe Ship.Ship
getShip shipId player =
  Fleet.getShip shipId player.fleet

nextNotAddedShipId : Player -> Maybe Int
nextNotAddedShipId player =
  let
    ship =
      player
        |> getShips
        |> List.filter (not << .added)
        |> List.head
  in
    case ship of
      Just s -> Just s.id
      Nothing -> Nothing

shoot : Loc.Location -> Player -> Player -> (Player, Player)
shoot pos player enemy =
  let
    shotCell = Grid.shoot pos enemy.primaryGrid
    trackingGrid = Grid.setCell pos shotCell player.trackingGrid
    primaryGrid = Grid.setCell pos shotCell enemy.primaryGrid
    updateIfSunk grid =
        case Fleet.getShipFromCoord pos enemy.fleet of
          Just ship ->
            if Grid.isShipDestroyed primaryGrid ship then
              Grid.sinkShip ship grid
            else grid
          Nothing -> grid
  in
    (,) { player | trackingGrid = updateIfSunk trackingGrid }
        { enemy | primaryGrid = updateIfSunk primaryGrid }

previewShip : Maybe Grid.Context -> Maybe Loc.Location -> Maybe Int -> Player -> Html.Html
previewShip clickHover maybeHoverPos maybeShipId player =
  let
    noPreview =
      player.primaryGrid
        |> Grid.toHtml clickHover
    preview ship =
      player.primaryGrid
        |> Grid.addShip ship
        |> Grid.toHtml clickHover
    invalid ship =
      player.primaryGrid
        |> Grid.addInvalidShip ship
        |> Grid.toHtml clickHover
  in
  case maybeShipId of
    Nothing -> noPreview
    Just shipId ->
      case maybeHoverPos of
        Nothing -> noPreview
        Just hoverPos ->
          case getShip shipId player of
            Nothing -> noPreview
            Just ship ->
              let
                shipToAdd = (Ship.setLocation hoverPos ship)
              in
                if canAddShip shipToAdd player then
                  preview shipToAdd
                else
                  invalid shipToAdd

viewTrackingGrid : Maybe Grid.Context -> Player -> Html.Html
viewTrackingGrid context player =
  Grid.toHtml context player.trackingGrid

viewPrimaryGrid : Maybe Grid.Context -> Player -> Html.Html
viewPrimaryGrid context player =
  Grid.toHtml context player.primaryGrid
