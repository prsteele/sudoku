{-# LANGUAGE TemplateHaskell #-}
module Sudoku.Puzzle where

import Control.Lens
import Control.Lens.TH

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
data Puzzle
  = Puzzle
    { _puzzleCells :: [Cell]
    , _puzzleGroups :: [Group]
    , _puzzleAlphabetSize :: Int
    , _puzzleCellGroups :: Cell -> [Group]
    }
makeLenses ''Puzzle

puzzleAlphabet :: Puzzle -> [Int]
puzzleAlphabet puzzle = [1..puzzle ^. puzzleAlphabetSize]

-- | A list of all Groups that contain a Cell.
--
-- This can be used to define the 'puzzleCellGroups' field for a
-- generic puzzle.
defaultCellGroups :: Cell -> Puzzle -> [[Cell]]
defaultCellGroups cell (Puzzle _ groups _ _)
  = filter inGroup groups
  where
    inGroup group = cell `elem` group

mkStandardPuzzle :: Puzzle
mkStandardPuzzle
  = Puzzle
  { _puzzleCells = curry Cell <$> alphabet <*> alphabet
  , _puzzleGroups = rows ++ columns ++ blocks
  , _puzzleAlphabetSize = 9
  , _puzzleCellGroups = cellGroups
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
