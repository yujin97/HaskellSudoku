 module Solve 
   (
      solve,
      gameIsEnd,
      isGridInvalid,
      pruneGrid
   )  where

import Grid
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Data.Function
import Control.Applicative
import Data.List.Zipper
 
-- More explanation of the features is available in the report.

-- Function for pruning a row of cell. Adapted from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
-- Eliminate possibilities of cell by set difference of fixed cells and possible list of a cell
-- No list difference means the cell cannot be assigned with any value. The game cannot proceed. So return Nothing.
pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = traverse pruneCell cells
  where
    fixeds = [ x | (Fixed x,_) <- cells]
    pruneCell ((Possible xs, block)) = case xs Data.List.\\ fixeds of
      []  -> Nothing
      [y] -> Just (Fixed y, block)
      ys  -> Just (Possible ys, block)
    pruneCell x = Just x
 
-- Function for changing a block into a row of cell.
subGridToRow' :: Grid -> Int -> Row
subGridToRow' grid num = [ (v,bn) | (v,bn) <- cells, num' == bn]
  where cells = concat grid
        num' = intToDigit num
 
-- Function for changing a grid into a grid that cells of same block placed in same row.
subGridToRow :: Grid -> Grid
subGridToRow grid = [subGridToRow' grid block |block <- [0..8]]
 
-- Function to prune cells by comparing cells with cells in same block. Adapted from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
-- Set difference is checked to update possible values of possible cells.
pruneCells' :: Grid -> Maybe Grid
pruneCells' grid = case (traverse pruneCell cells) of
    Just a -> Just (Data.List.Split.chunksOf 9 a)
    Nothing -> Nothing
  where
    cells = concat grid
    pruneCell ((Possible xs, block)) = case xs Data.List.\\ fixeds of
       []  -> Nothing
       [y] -> Just (Fixed y, block)
       ys  -> Just (Possible ys, block)
       where fixeds = [ x | (Fixed x, bn) <- cells, bn == block]
    pruneCell x = Just x
 
-- Function to prune a whole grid. Adapted from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid =
   traverse pruneCells grid
   >>= fmap Data.List.transpose . traverse pruneCells . Data.List.transpose
   >>= pruneCells'

-- Function to check whether a grid is filled.
-- In this game desing, filled means wins.
gameIsEnd :: Grid -> Bool
gameIsEnd grid = length possible == 0
  where possible = [ a | (Possible a, v)<- concat grid]

-- Function for pruning a grid until the value is fixed. Adapted from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
pruneGrid :: Grid -> Maybe Grid
pruneGrid = untilFixed pruneGrid'
  where
    untilFixed f x = f x >>= \x' -> if x' == x then return x else untilFixed f x'


-- Function for guessing a possible move. Adapted from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
-- i is the index of the target cell
-- index = [0..89]
-- isPossible : Check a cell is a possible cell
-- fixCell : generate cell tuple with index. One of the cell must be fixed.
-- replace2D: changing value of a cell inside a grid
nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _, _), rest) =
        fixCell
        . Data.List.minimumBy (compare `Data.Function.on` (possibilityCount . snd))
        . filter (isPossible . snd)
        . zip [0..] -- binding index with cell 
        . concat
        $ grid
  in (replace2D i first grid, replace2D i rest grid)
  where
    isPossible (Possible _, _) = True
    isPossible _            = False
    possibilityCount (Possible xs, _) = length xs
    possibilityCount (Fixed _, _)     = 1
    fixCell (i, (Possible [x, y], bn)) = (i, (Fixed x,bn), (Fixed y,bn)) -- cell with 2 possibilities
    fixCell (i, (Possible (x:xs), bn)) = (i, (Fixed x,bn), (Possible xs,bn)) -- cell with more than 2 possibilities
    fixCell _                    = error "Impossible case"
    replace2D :: Int -> a -> [[a]] -> [[a]] -- insert value into the grid
    replace2D i v =
      let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]
  

-- Function for checking whether a grid is valid. Adapted from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
-- Grid cannot has possible cell with 0 possible value.
isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidRow grid -- check Row
  || any isInvalidRow (Data.List.transpose grid) -- check column
  || any isInvalidRow (subGridToRow grid) -- check sub-grid
  where
    isInvalidRow row =
      let fixeds         = [v | (Fixed v,_) <- row]
          empties = [v | (Possible v,_) <- row, v == []]
      in isDuplicated fixeds || not (null empties)
    isDuplicated l = isDuplicated' l [] -- Check duplication in list
    isDuplicated' [] _ = False
    isDuplicated' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = isDuplicated' ys (y:xs)

-- Function for solving a Sudoku game. Adapted from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/
-- more explanation in report
solve :: Grid -> Maybe Grid
solve grid = pruneGrid grid >>= solve'
  where
    solve' grid'
      | isGridInvalid grid' = Nothing -- Check invalid
      | gameIsEnd grid'  = Just grid' -- Check game end
      | otherwise       =
          let (nextGrid1, nextGrid2) = nextGrids grid'
          in solve nextGrid1 <|> solve nextGrid2 -- return one of the grid. Just > Nothing