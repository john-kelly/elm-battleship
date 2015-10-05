module Player where

-- Core
-- Evan
-- 3rd Party
-- Battleship
import Fleet
import Grid

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
