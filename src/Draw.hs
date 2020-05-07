module Draw 
(
  drawGrid
) where

import Grid

-- Draw the Sudoku grid.
drawGrid :: Grid -> String
drawGrid grid = (drawFirstRow (grid !! 0)) ++ "\n" ++ (drawGrid' grid) ++ (drawLastCellsLower (grid !! 8))

-- Helper Function. Draw 2-9 row of a grid.
drawGrid' :: [Row] -> String
drawGrid' (_:[]) = []
drawGrid' (row1:row2:rest) = (drawRowNormal row1 row2) ++ drawGrid' (row2:rest)

-- Helper Function. Draw 1st row of a grid.
drawFirstRow :: Row -> String
drawFirstRow row =upper ++ "\n" ++ "|" ++ (drawRow row)
  where
    upper = drawFirstCellsUpper row

-- Helper Function. Draw a row that is not the first row.
drawRowNormal :: Row -> Row -> String
drawRowNormal row1 row2 = drawCellsUpper [row1,row2] ++ "\n" ++ "|" ++ (drawRow row2) ++ "\n" 

-- Helper Function. Draw the first row,
drawRow :: Row -> String
drawRow [] = []
drawRow (x:y:xs) = drawCell (x:[y]) ++ drawRow (y:xs)
drawRow (x:[]) = drawCell (x:[])

-- Helper Function for drawing a cell of the first row. Need to be determine by 2 cells.
drawCell :: [Cell] -> String
drawCell ((cell, block):[]) = " " ++ (showCell cell) ++ " |"
drawCell ((value1, block1):(value2, block2):[])
  | block1 == block2 = " " ++ (showCell value1) ++ "  "
  | otherwise = " " ++ (showCell value1) ++ " |"

-- Helper Function. Show the value of a cell.
showCell :: CellV -> String
showCell (Fixed x) = show x
showCell _ = "."

-- Helper Function. Draw the lower border of the last row of the grid.
drawLastCellsLower :: Row -> String
drawLastCellsLower row = ('\'':(drawLastCellsLower' row))

-- Helper Function of drawFirstCellsUpper
drawLastCellsLower' :: Row -> String
drawLastCellsLower' (x:[]) = "---'"
drawLastCellsLower' ((value1, block1) :(value2, block2):xs)
  | block1 == block2 = "----" ++ drawLastCellsLower' ((value2, block2):xs)
  | otherwise = "---'" ++ drawLastCellsLower' ((value2, block2):xs)
  
-- Helper Function. Draw the upper border of the first row of the grid.
drawFirstCellsUpper :: Row -> String
drawFirstCellsUpper row = ('.':(drawFirstCellsUpper' row))

-- Helper Function of drawFirstCellsUpper
drawFirstCellsUpper' :: Row -> String
drawFirstCellsUpper' (x:[]) = "---."
drawFirstCellsUpper' ((value1, block1) :(value2, block2):xs)
  | block1 == block2 = "----" ++ drawFirstCellsUpper' ((value2, block2):xs)
  | otherwise = "---." ++ drawFirstCellsUpper' ((value2, block2):xs)

-- Function for drawing the upper border of row.
drawCellsUpper :: [Row] -> String
drawCellsUpper rows@(((value1,block1):rest1) : ((value2,block2):rest2) : [])
  | block1 /= block2 = ":" ++ (drawCellsUpper' rows)
  | otherwise = "|" ++ (drawCellsUpper' rows)

-- Heler function of drawCellsUpper
-- Draw the upper border of row that is not the first row
-- two rows are needed for the function
-- as the border are determined by block number of three neighbors
-- neighbors: next cell, upper cell, upper next cell.
drawCellsUpper' :: [Row] -> String
drawCellsUpper' (_:[]) = []
drawCellsUpper' (((value1,block1):[]) : ((value2,block2):[]) : [])
 | block1 /= block2 = "---:"
 | otherwise = "   |"
drawCellsUpper' (((value1,block1):(value2,block2):rest1) : ((mvalue,mblock):(mvalue1,mblock1):mrest) : [])
 | block1 /= mblock && block1 /= block2 && mblock /= mblock1 = "---:" ++ drawCellsUpper' [((value2,block2):rest1), ((mvalue1,mblock1):mrest)]
 | block1 /= block2 && mblock /= mblock1 = "   |" ++ drawCellsUpper' [((value2,block2):rest1), ((mvalue1,mblock1):mrest)]
 | block1 /= block2 && mblock == mblock1 && block1 /= mblock = "---'" ++ drawCellsUpper' [((value2,block2):rest1), ((mvalue1,mblock1):mrest)]
 | block1 /= block2 && mblock == mblock1 = "   '" ++ drawCellsUpper' [((value2,block2):rest1), ((mvalue1,mblock1):mrest)]
 | block1 == block2 && mblock /= mblock1 && block1 /= mblock = "---." ++ drawCellsUpper' [((value2,block2):rest1), ((mvalue1,mblock1):mrest)]
 | block1 == block2 && mblock /= mblock1 = "   ." ++ drawCellsUpper' [((value2,block2):rest1), ((mvalue1,mblock1):mrest)]
 | block1 /= mblock1 = "----" ++ drawCellsUpper' [((value2,block2):rest1), ((mvalue1,mblock1):mrest)]
 | otherwise = "    " ++ drawCellsUpper' [((value2,block2):rest1), ((mvalue1,mblock1):mrest)]