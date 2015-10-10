module Location
  ( Location
  , init
  , row
  , column
  , addToRow
  , addToColumn
  )
  where

-- NOTE intended to just be a light weight wrapper around a tuple. Using the
-- tuple literal syntax should not be considered breaking the abstraction layer

-- Core
-- Evan
-- 3rd Party
-- Battleship

type alias Location = (Int, Int)

init : Int -> Int -> Location
init = (,)

row : Location -> Int
row = fst

column : Location -> Int
column = snd

addToRow : Int -> Location -> Location
addToRow toAdd location =
  (((row location) + toAdd), (column location))

addToColumn : Int -> Location -> Location
addToColumn toAdd location =
  ((row location), ((column location) + toAdd))
