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

type alias ShipId = Int

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

addShip : ShipId -> Player -> Player
addShip shipId player =
  case Fleet.getShip shipId player.fleet of
    Just ship ->
      if canAddShip ship player then
        { player |
            -- This is important here. Both the ship in the fleet and the grid
            -- are updated when a ship is added.
            fleet <- Fleet.updateShip shipId Ship.setAddedTrue player.fleet,
            primaryGrid <- Grid.addShipCoords (Ship.getShipCoordinates ship) player.primaryGrid
        }
      else
        player
    Nothing -> player

canAddShip : Ship.Ship -> Player -> Bool
canAddShip ship player =
  -- order here is important for optimization. `shipInBounds` is cheap
  if | not (Grid.coordsInBounds (Ship.getShipCoordinates ship) player.primaryGrid) -> False
     | Fleet.shipOverlaps ship player.fleet -> False
     | otherwise -> True

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

updateShip : ShipId -> (Ship.Ship -> Ship.Ship) -> Player -> Player
updateShip shipId fn player =
  let
    newShip = Fleet.updateShip shipId fn player.fleet
  in
    { player | fleet <- newShip }

updateGrid : Grid.Grid -> Player -> Player
updateGrid grid player =
  { player | primaryGrid <- grid }

getShips : Player -> List Ship.Ship
getShips player =
  player.fleet
    |> Fleet.toList

nextNotAddedShipId : Player -> Maybe ShipId
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
    isSunk =
      --if shotCell == Grid.(Ship True) then
        -- Check if the ship has been sunk
        -- Find ship from position
        enemy.fleet
          |> Fleet.toList
          |> List.map (\s -> (Ship.hasCoordinate pos s, s))
          |> List.map (\(hasCoord, s) -> if hasCoord then Grid.isShipDestroyed primaryGrid s else False)
          |> List.any identity
      --else
      --  False
    updateIfSunk grid =
      if isSunk then
        enemy.fleet
          |> Fleet.toList
          |> List.foldr (\ship grid -> if Ship.hasCoordinate pos ship then Grid.sinkShip ship grid else grid) grid
      else
        grid
    trackingGrid = Grid.setCoord shotCell pos player.trackingGrid
    primaryGrid = Grid.setCoord shotCell pos enemy.primaryGrid
  in
    (,) { player | trackingGrid <- updateIfSunk trackingGrid }
        { enemy | primaryGrid <- updateIfSunk primaryGrid }

previewShip : Maybe Grid.Context -> Maybe Loc.Location -> Maybe ShipId -> Player -> Html.Html
previewShip clickHover maybeHoverPos maybeShipId player =
  let
    noPreview =
      Html.div []
        [ player.primaryGrid
            |> Grid.toHtml clickHover
        ]
    preview ship =
      Html.div []
        [ player.primaryGrid
            |> Grid.addShipCoords (Ship.getShipCoordinates ship)
            |> Grid.toHtml clickHover
        ]
    invalid ship =
      Html.div []
        [ player.primaryGrid
            |> Grid.addInvalidCoords (Ship.getShipCoordinates ship)
            |> Grid.toHtml clickHover
        ]
  in
  case maybeShipId of
    Nothing -> noPreview
    Just shipId ->
      case maybeHoverPos of
        Nothing -> noPreview
        Just hoverPos ->
          case Fleet.getShip shipId player.fleet of
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
  Html.div [] [ Grid.toHtml context player.trackingGrid ]

viewPrimaryGrid : Maybe Grid.Context -> Player -> Html.Html
viewPrimaryGrid context player =
  Html.div [] [ Grid.toHtml context player.primaryGrid ]
