module Location where

-- Core
-- Evan
-- 3rd Party
-- Battleship

type alias Location = (Int, Int)

init : Int -> Int -> Location
init row column =
  (row, column)

row : Location -> Int
row location =
  fst location

addToRow : Int -> Location -> Location
addToRow toAdd location =
  init ((row location) + toAdd) (column location)

column : Location -> Int
column location =
  snd location

addToColumn : Int -> Location -> Location
addToColumn toAdd location =
  init (row location) ((column location) + toAdd)
