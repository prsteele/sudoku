{-# LANGUAGE TupleSections #-}
module Sudoku.Search where

import qualified Data.Map.Strict as M

import Sudoku.Puzzle
import Sudoku.Rules

data Eligibility
  = Eligible
  | Candidate
  | Ineligible

search :: a -> (a -> [a]) -> (a -> Eligibility) -> [a]
search state candidates check
  = case check state of
      Eligible   -> state : remainder
      Candidate  -> remainder
      Ineligible -> []
  where
    children = candidates state
    remainder = concat [search child candidates check | child <- children]

nextCandidates :: Puzzle -> Contents -> [Contents]
nextCandidates puzzle contents
  = emptyCells puzzle contents >>= cellCompletions puzzle contents

puzzleEligibility :: Puzzle -> Contents -> Eligibility
puzzleEligibility puzzle contents = case puzzleStatus puzzle contents of
  Complete   -> Eligible
  Incomplete -> Candidate
  Witness _  -> Ineligible

solvePuzzle :: Puzzle -> [Contents]
solvePuzzle puzzle = snd <$> search initialState candidates eligibility
  where
    initialState :: (Puzzle, Contents)
    initialState = (puzzle, Contents (M.empty))

    candidates :: (Puzzle, Contents) -> [(Puzzle, Contents)]
    candidates = fmap (puzzle,) . uncurry nextCandidates

    eligibility :: (Puzzle, Contents) -> Eligibility
    eligibility = uncurry puzzleEligibility
