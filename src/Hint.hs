module Hint 
  (
   hint
  ) where

import Grid
import Data.List
import Data.Char
import Data.Function
import Solve

-- main function of hint feature
-- Function suggest a possible move that does not violate Sudoku Rules
-- The value may not be correct as it suggest possible move based on current game state
-- Possible move is indicated by row, column and value
-- return Nothing when there is no possible valid move. It indicates the game is played wrongly.
hint :: Grid -> Maybe(Int,Int,[Int])
hint grid = case pruneGrid grid of
    Nothing -> Nothing
    Just grid' -> Just (hint' grid grid')

-- Determine whether there is certain fixed value based the current game state.  
hint' :: Grid -> Grid -> (Int,Int,[Int])
hint' grid1 grid2 = case fixed2 Data.List.\\ fixed1 of
     [] -> hint'' grid2
     _ ->  hintFixed grid1 grid2
  where
    fixed1 = [ (Fixed v, bn) | (Fixed v, bn) <- (concat grid1)]
    fixed2 = [ (Fixed v, bn) | (Fixed v, bn) <- (concat grid2)]

-- Handle cases where fixed value is not increased after pruning
-- Will suggest possible values of cell with least possible value
hint'' :: Grid -> (Int,Int,[Int])
hint'' grid = (i `quot` 9, i `mod` 9, values)
  where
       (i, (Possible values, bn)) = Data.List.minimumBy (compare `Data.Function.on` (countPossibility . snd)) possibles
       countPossibility (Possible cells, _) = length cells
       possibles = filter (isPossible) (zip [0..] (concat grid))
       isPossible (_,(Possible _, _)) = True
       isPossible _ = False

-- Handle cases when fixed value increased
-- suggest one of the fixed value
hintFixed :: Grid -> Grid -> (Int, Int, [Int])
hintFixed grid1 grid2 = (i `quot` 9, i `mod` 9, [value])
  where
       (i, (Fixed value, bn)) = (fixed2 Data.List.\\ fixed1) !! 0
       fixed1 = filter (isFixed) (zip [0..] (concat grid1))
       fixed2 = filter (isFixed) (zip [0..] (concat grid2))
       isFixed (_,(Fixed _, _)) = True
       isFixed _ = False