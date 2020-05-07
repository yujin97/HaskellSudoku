module UndoRedo
  (
      Game (..),
      undo,
      redo,
      update
  ) where



-- Storing history of game
data Game a = Game [a] [a]

-- Undo/ Redo
-- Based on https://stackoverflow.com/questions/40411645/is-it-useful-to-create-a-collection-of-generic-function-types-in-haskell

-- undo a game. More explanation in report
undo :: Game a -> Game a
undo game@(Game (first:[]) _) = game
undo (Game (first:rest) redoStack) = Game rest (first:redoStack)
undo game = game

-- redo a game. More explanation in report
redo :: Game a -> Game a
redo (Game record (last:redoStack)) = Game (last:record) redoStack 
redo game = game

-- update a game. More explanation in report
update :: (a -> (int,int) -> int -> a) -> (int,int) -> int -> Game a -> Game a
update change (col,row) val (Game (x:xs) _) = Game ((change x (col,row) val):x:xs) []