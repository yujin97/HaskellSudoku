module Read 
(
  readGrid
) where

import Grid
import Data.List.Split
import Data.Char

-- Read String and transform that to a Sudoku Grid. Adapted from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
-- Return Nothing if any of the cell return Nothing
-- Traverse each cells in each row then traverse each row in the grid.
-- Return nothing if the size of the grid is not 179 or 180 (referencing format of map.txt, map2.txt)
readGrid :: String -> Maybe Grid
readGrid grid
    | (length grid == 180 || length grid == 179) && (isJigSaw grid)= traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ (splitMap grid)
    | otherwise      = Nothing
    where
    readCell (block,'.') = Just (Possible [1..9], block)
    readCell (block, c)
        | Data.Char.isDigit c && c > '0' = Just (Fixed (digitToInt c), block)
        | otherwise = Nothing

-- Check that each block has 9 cells.
isJigSaw :: String -> Bool
isJigSaw grid = foldr (&&) True [isJigSaw' grid bn| bn <- ['0'..'8']]
    where
    isJigSaw' grid' bn' = (length [ cell |cell <- (take 90 grid), cell == bn']) == 9

-- Store each cell as (Block number, value) pair.
splitMap :: String -> [(Char, Char)]
splitMap [] = []
splitMap map = zip (splitMap' map !! 0) (splitMap' map !! 1)

-- Helper Function. Transform string to 2 strings of cell's value and block number.
splitMap' :: String -> [String]
splitMap' [] = []
splitMap' map = Data.List.Split.chunksOf 81 (concat (splitMap'' map))

-- Helper Function. Take string from readFile and remove the newlines.
splitMap'' :: String -> [String]
splitMap'' [] = []
splitMap'' map
    | length map == 179 = [init row |row <- (init (Data.List.Split.chunksOf 10 map))] ++ [(last (Data.List.Split.chunksOf 10 map))]
    | length map == 180 = [init row |row <- (Data.List.Split.chunksOf 10 map)]