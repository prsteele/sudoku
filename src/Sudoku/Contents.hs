module Sudoku.Contents where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.List (sort)
import Data.List.Ordered (nubSort)
import Data.List.NonEmpty (NonEmpty((:|)))

import Sudoku.Puzzle

type Value = Int

-- | The contents of a puzzle.
newtype Contents = Contents (M.Map Cell Value)
  deriving
    ( Show
    )

-- | Get the entries of a group.
readGroup :: Contents -> Group -> [Value]
readGroup (Contents contents) group
  = catMaybes (flip M.lookup contents <$> group)

-- | The status of a group.
--
-- A group can either be complete and correct, incomplete (but
-- admitting possible completions), or have a witness to an illegal
-- configuration.
data GroupStatus
  = Complete
  | Incomplete
  | Contradiction (NonEmpty Int)
  deriving
    ( Show
    )

-- | Compute the status of a group.
groupStatus :: Puzzle -> Contents -> Group -> GroupStatus
groupStatus puzzle contents group
  | values == puzzleAlphabet puzzle = Complete
  | otherwise = case repeated of
      []      -> Incomplete
      (x: xs) -> Contradiction (x :| xs)
  where
    values :: [Int]
    values = sort (readGroup contents group)

    complete :: [Int]
    complete = [1..puzzle ^. puzzleAlphabetSize]

    repeated :: [Int]
    repeated = map fst $ filter (uncurry (==)) (zip values (tail values))
