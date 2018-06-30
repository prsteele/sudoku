module Sudoku.Puzzle where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Text.Printf

-- | A coordinate of a puzzle
newtype Cell = Cell (Int, Int)
  deriving
    ( Show
    , Eq
    , Ord
    )

-- | A list of cells that must have unique contents
type Group = [Cell]

-- | A description of a puzzle layout.
data Puzzle a
  = Puzzle
    { puzzleCells :: [Cell]
    , puzzleGroups :: [Group]
    , puzzleAlphabet :: [a]
    , puzzleCellGroups :: Cell -> [Group]
    }

-- | A list of all Groups that contain a Cell.
--
-- This can be used to define the 'puzzleCellGroups' field for a
-- generic puzzle.
defaultCellGroups :: Cell -> Puzzle a -> [[Cell]]
defaultCellGroups cell (Puzzle _ groups _ _)
  = filter inGroup groups
  where
    inGroup group = cell `elem` group

-- | The contents of a puzzle.
newtype Contents a = Contents (M.Map Cell a)
  deriving
    ( Show
    )

groupEntries :: Puzzle a -> Contents a -> [[a]]
groupEntries (Puzzle _ groups _ _) (Contents contents)
  = groupEntries' <$> groups
  where
    groupEntries' cells = catMaybes [contents M.!? cell | cell <- cells]

mkStandardPuzzle :: Puzzle Int
mkStandardPuzzle
  = Puzzle
  { puzzleCells = curry Cell <$> alphabet <*> alphabet
  , puzzleGroups = rows ++ columns ++ blocks
  , puzzleAlphabet = [0..8]
  , puzzleCellGroups = cellGroups
  }
  where
    alphabet = [0..8]
    blockStarts = filter ((== 0) . (`div` 3)) alphabet

    rows =
      [ [ Cell (row, column)
        | column <- alphabet
        ]
      | row <- alphabet
      ]
    columns =
      [ [ Cell (row, column)
        | row <- alphabet
        ]
      | column <- alphabet
      ]
    blocks =
      [ [ Cell (row + i, column + j)
        | i <- [0..2]
        , j <- [0..2]
        ]
      | row <- blockStarts
      , column <- blockStarts
      ]

    cellGroups :: Cell -> [[Cell]]
    cellGroups (Cell (row, column))
      = cellRow : cellColumn : [cellBlock]
      where
        cellRow = [Cell (row, column') | column' <- alphabet]
        cellColumn = [Cell (row', column) | row' <- alphabet]
        cellBlock =
          [ Cell (row' + i, column' + j)
          | i <- [0..2]
          , j <- [0..2]
          ]
          where
            row' = (row `div` 3) * 3
            column' = (column `div` 3) * 3
