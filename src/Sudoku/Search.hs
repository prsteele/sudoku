{-# LANGUAGE TupleSections #-}
module Sudoku.Search where

import Control.Monad
import Control.Monad.Reader
import Data.List (sortOn)
import qualified Data.Map.Strict as M

import Sudoku.Puzzle
import Sudoku.Rules

data Eligibility
  = Eligible
  | Candidate
  | Ineligible

type Search = Reader Puzzle

search :: a -> (a -> Search [a]) -> (a -> Search Eligibility) -> Search [a]
search state candidates check = do
  eligibility <- check state
  children <- candidates state
  remainders <- forM children $ \child -> search child candidates check
  return $ case eligibility of
             Eligible   -> state : concat remainders
             Candidate  -> concat remainders
             Ineligible -> []

nextCandidates :: Contents -> Search [Contents]
nextCandidates contents =
  let
    cells puzzle = emptyCells puzzle contents
    completions puzzle = cellCompletions puzzle contents <$> (cells puzzle)
  in do
    puzzle <- ask
    return $ concat (sortOn length (completions puzzle))

puzzleEligibility :: Contents -> Search Eligibility
puzzleEligibility contents = do
  puzzle <- ask
  case puzzleStatus puzzle contents of
    Complete   -> return Eligible
    Incomplete -> return Candidate
    Witness _  -> return Ineligible

solvePuzzle :: Puzzle -> Contents -> [Contents]
solvePuzzle puzzle start
  = runReader (search start nextCandidates puzzleEligibility) puzzle
