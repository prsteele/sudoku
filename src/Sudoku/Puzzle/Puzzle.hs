{-# LANGUAGE TemplateHaskell #-}
module Sudoku.Puzzle.Puzzle where

import Control.Lens
import Control.Lens.TH

-- | A coordinate of a puzzle
newtype Cell = Cell { unCell :: (Int, Int) }
  deriving
    ( Show
    , Eq
    , Ord
    )

-- | A list of cells that must have unique contents
newtype Group = Group { unGroup :: [Cell] }
  deriving
    ( Show
    , Eq
    )

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
defaultCellGroups :: Puzzle -> Cell -> [Group]
defaultCellGroups (Puzzle _ groups _ _) cell
  = filter (inGroup . unGroup) groups
  where
    inGroup group = cell `elem` group

