module Sudoku.Rules where

import Control.Lens
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.Ordered (minus, nubSort)
import Data.Semigroup

import Sudoku.Puzzle

-- | A witness to an invalid group.
data Contradiction = Contradiction Group CellValue
  deriving
    ( Show
    , Eq
    )

-- | The status of a puzzle or group.
--
-- A puzzle or group can either be complete, incomplete, or have a
-- witness to an illegal configuration.
data Status
  = Complete
  | Incomplete
  | Witness (NonEmpty Contradiction)
  deriving
    ( Show
    , Eq
    )

-- | Combine statuses.
--
-- If either status has witnesses, the result is a witness with the
-- combined contradictions. Otherwise, if either status is incomplete
-- then the result is incomplete. Finally, if both statuses are
-- Complete then the result is Complete.
instance Semigroup Status where
  (Witness cs) <> (Witness cs') = Witness (cs <> cs')
  _            <> (Witness cs)  = Witness cs
  (Witness cs) <> _             = Witness cs
  _            <> Incomplete    = Incomplete
  Incomplete   <> Complete      = Incomplete
  Complete     <> Complete      = Complete

-- | Compute the status of a group.
groupStatus :: Puzzle -> Contents -> Group -> Status
groupStatus puzzle contents group
  | values == puzzleAlphabet puzzle = Complete
  | otherwise = case contradictions of
      (c: cs) -> Witness (c :| cs)
      []      -> Incomplete
  where
    values :: [Int]
    values = sort (readGroup contents group)

    complete :: [Int]
    complete = [1..puzzle ^. puzzleAlphabetSize]

    repeated :: [Int]
    repeated = map fst $ filter (uncurry (==)) (zip values (tail values))

    contradictions = Contradiction group <$> repeated

puzzleStatus :: Puzzle -> Contents -> Status
puzzleStatus puzzle contents = sconcat (Complete :| statuses)
  where
    statuses = groupStatus puzzle contents <$> puzzle ^. puzzleGroups

-- | The empty cells in a puzzle
emptyCells :: Puzzle -> Contents -> [Cell]
emptyCells puzzle contents = filter missing (puzzle ^. puzzleCells)
  where
    missing = (== Nothing) . readCell contents

-- | Compute possible candidates of a cell value
cellCandidates :: Puzzle -> Contents -> Cell -> [CellValue]
cellCandidates puzzle contents cell
  = minus (puzzleAlphabet puzzle) present
  where
    groups :: [Group]
    groups = puzzle ^. puzzleCellGroups $ cell

    present :: [CellValue]
    present = (nubSort . concat) (readGroup contents <$> groups)

cellCompletions :: Puzzle -> Contents -> Cell -> [Contents]
cellCompletions puzzle contents cell
  = fillCell contents cell <$> candidates
  where
    candidates = cellCandidates puzzle contents cell
