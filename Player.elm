module Player where

-- Core
-- Evan
-- 3rd Party
-- Battleship

-- Player
type alias Player =
    { ships : Dict.Dict Int Ship
    , primaryGrid : Grid
    , trackingGrid : Grid
    }
defaultPlayer : Player
defaultPlayer =
    { ships = defaultShips
    , primaryGrid = defaultGrid
    , trackingGrid = defaultGrid
    }
-- TODO Setup a random board for the computer.
-- This will be different than the defaultPlayer function.
defaultComputer : Player
defaultComputer =
    { ships = defaultShips
    , primaryGrid = defaultGrid
    , trackingGrid = defaultGrid
    }
