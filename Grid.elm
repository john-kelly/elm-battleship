module Grid where

-- Core
-- Evan
-- 3rd Party
import Matrix
-- Battleship


-- Grid
type alias IsHit = Bool
type alias Grid = Matrix.Matrix Cell
type Cell
    = Ship IsHit
    | Empty IsHit
    | Unknown

defaultPrimaryGrid : Grid
defaultPrimaryGrid =
  Matrix.repeat 10 10 (Empty False)

defaultTrackingGrid : Grid
defaultTrackingGrid =
  Matrix.repeat 10 10 Unknown
