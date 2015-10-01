module Fleet where

-- Core
-- Evan
-- 3rd Party
-- Battleship

import Ship

defaultShips : Dict.Dict Int Ship.Ship
defaultShips =
    Dict.fromList
    [ (1, Ship 2 Horizontal 0 0 False)
    , (2, Ship 3 Horizontal 0 0 False)
    , (3, Ship 4 Horizontal 0 0 False)
    ]
