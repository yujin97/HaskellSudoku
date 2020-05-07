module Grid 
(
  CellV (..),
  Cell,
  Row,
  Grid
) where

-- Data Type for Sudoku Game
data CellV = Fixed Int | Possible [Int] deriving (Show, Eq)
type Cell = (CellV, Char)
type Row  = [Cell]
type Grid = [Row]