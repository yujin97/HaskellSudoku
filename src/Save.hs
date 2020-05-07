module Save
(
  writeGrid
) where

import Grid
import Data.Char

-- Function for transforming a grid into string.
writeGrid :: Grid -> String
writeGrid grid = (writeBlock grid) ++ (writeValue grid)

-- Function for transforming structure of grid into string
writeBlock :: Grid -> String
writeBlock grid = block
  where
    block =  concat [ writeBlock' row| row <- grid]

-- Helper function of writeBlock. Adding new line to each row.
writeBlock' :: Row -> String
writeBlock' row = block ++ "\n"
  where
    block = [ block |(_, block)<- row]

-- Function for transforming value of grid into string
writeValue :: Grid -> String
writeValue grid = concat [ writeValue' row | row <- grid ]

-- Helper function of writeValue. Adding new line to each row.
writeValue' :: Row -> String
writeValue' row = value ++ "\n"
  where
    readCell (Possible v, _) = '.'
    readCell (Fixed v, _) = intToDigit v
    value = [ readCell cell | cell <- row]
