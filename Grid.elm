module Grid where

-- Core
-- Evan
-- 3rd Party
import Matrix
-- Battleship


-- Grid
type alias Grid = Matrix.Matrix Cell
type Cell
    = HitCell
    | MissCell
    | DefaultCell
defaultGrid : Grid
defaultGrid =
    Matrix.repeat 10 10 DefaultCell
