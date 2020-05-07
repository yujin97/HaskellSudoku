module Sudoku 
  (  startSudoku
  ) where

import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Control.Monad.Fix
import Data.Function
import Control.Applicative
import Data.List.Zipper
import Grid
import Draw
import Read
import Save
import Solve
import UndoRedo
import Hint

-- Function to check whether a move align the rule of Sudoku.
isValidMove :: Grid -> (Int, Int) -> Int -> Bool
isValidMove grid (x,y) value = withinBound && notOccupy (x,y) && isValidRow && isValidCol && isValidBlock
  where
    withinBound = (elem x [0..8]) && (elem y [0..8]) && (elem value [1..9])
    notOccupy (x',y') = case (grid !! y' !! x') of
      (Fixed _, _) -> False
      (Possible _, _) -> True
    isValidRow = not (elem value row)
      where row = [ v| (Fixed v, bn) <- grid !! y]
    isValidCol = not (elem value ([ v |(Fixed v,_) <- col]))
      where col = [ grid !! row !! x | row <- [0..8]]
    isValidBlock = not (elem value block)
      where
        num = getBlock (grid !! y !! x)
        block = [ v |(Fixed v, bn) <- concat grid, bn == num]

-- Function to check whether data input for a move a valid.
isValidInput :: String -> String -> String -> Bool
isValidInput x y v = lengthCheck && valueCheck
  where 
    lengthCheck = (length x == 1) && (length y == 1) && (length v == 1)
    valueCheck = (elem ((x) !! 0) ['0'..'8']) && (elem (y !! 0) ['0'..'8']) && (elem (v !! 0) ['1'..'9'])

-- Function to get block number of a cell.
getBlock :: Cell -> Char
getBlock (v, bn) = bn

-- Function for writing a cell to a grid.
putCell :: Grid -> (Int, Int) -> Int -> Grid
putCell grid (x, y) v = init one ++ [(putCell' (last one) x v)] ++ two 
  where
       (one, two) = splitAt (y + 1) grid

-- Helper function of putCell
-- inserting value to list
putCell' :: Row -> Int -> Int -> Row
putCell' row x v = init one ++ [(Fixed v, (getBlock (last one)))] ++ two 
  where
       (one, two)  = splitAt (x + 1) row

-- Starter function called by the main
startSudoku :: IO()
startSudoku = do
  putStrLn "Please Select a command from 'load and 'quit'."
  command <- getLine
  case command of
      "load" -> do loadGrid
      "quit" -> do putStrLn "Bye. Thank you for playing!"
      _ -> do
        putStrLn "Invalud command. Try again."
        startSudoku

-- load function
loadGrid :: IO ()
loadGrid = do
        putStrLn "Please select a sudoku game"
        file <- getLine
        words <- readFile file
        case readGrid words of
            Just grid -> do 
                        if (not (isGridInvalid grid))
                          then do
                              putStrLn "Map is loaded successfully!"
                              putStrLn "The initial map:"
                              putStrLn (drawGrid grid)
                              opt (Game [grid] [])
                            else do
                                putStrLn "The Sudoku game is not valid."
                                startSudoku
            Nothing -> do 
                   putStrLn "The Sudoku game is not valid."
                   startSudoku

-- Selector function for user to input command
opt :: (Game Grid) -> IO()
opt grid@(Game record@(current:rest) popped) = do
    putStrLn "\nPlease select your action."
    commandStr <- getLine
    let command = (words commandStr) !! 0
    case command of
        "load" -> do
              loadGrid
        "play" -> do 
              play grid
        "quit" -> do
              putStrLn "Bye. Thank you for playing!"
        "redo" -> do
              putStrLn "Redo the Sudoku board..."
              let redone@(Game (current':rest') popped') = redo grid 
              putStrLn "The current grid:"
              putStrLn (drawGrid current')
              opt redone
        "undo" -> do
              putStrLn "Undo the Sudoku board..."
              let undone@(Game (current':_) popped') = undo grid 
              putStrLn "The current grid:"
              putStrLn (drawGrid current')
              opt undone
        "save" -> do
              putStrLn "Please specify the file path."
              fileName <- getLine
              let game = writeGrid current
              writeFile ((words fileName) !! 0) game
              putStrLn "Your game is saved."
              opt grid
        "solve" -> do
                putStrLn "Solving... It may take some time."
                case (solve (last record)) of
                    Just sol -> do
                        putStrLn (drawGrid sol)
                        putStrLn "Done."
                        opt grid
                    Nothing -> do 
                          putStrLn "No solution."
                          opt grid
        "hint" -> do
              putStrLn "The hint is not certainly correct. But it is reasonable for current game state"
              case (hint current) of
                  Nothing -> do
                         putStrLn "There is no possible move. You should undo your move."
                  Just (row, col, values) -> do
                      putStrLn "Possible move: "
                      putStrLn ("Row: " ++ [intToDigit row])
                      putStrLn ("Column: " ++ [intToDigit col])
                      putStrLn ("Possibilities: " ++ (show values))
                      opt grid
        _ -> do
          putStrLn "Please input command from \"play, save, redo, undo, hint, solve, quit\"."
          opt grid

-- Function for making move. Need to specify row number, col number and value.
play :: (Game Grid) -> IO()
play gh@(Game (current:rest) popped) = do
    putStrLn "\nMake a move."
    putStrLn "The current grid:"
    putStrLn (drawGrid current)
    putStrLn "Row:"
    y' <- getLine
    let y = (words y') !! 0 !! 0
    putStrLn "\nColumn:"
    x' <- getLine
    let x = (words x') !! 0 !! 0
    putStrLn "\nValue:"
    v' <- getLine
    let v = (words v') !! 0 !! 0
    if (isValidInput x' y' v') && (isValidMove current ((digitToInt x),(digitToInt y)) (digitToInt v))
      then do
          let gridHistory = update putCell ((digitToInt x),(digitToInt y)) (digitToInt v) gh
          let grid' = putCell current ((digitToInt x),(digitToInt y)) (digitToInt v)
          if (gameIsEnd grid')
            then do
                putStrLn ""
                putStrLn (drawGrid grid')
                putStrLn "\nCongrats! You won. You have finished the sudoku!"
          else do 
              putStrLn "\nThe current grid:"
              putStrLn (drawGrid grid')
              opt gridHistory
      else do 
          putStrLn "\nYour input was invalid, please input again!"
          opt gh