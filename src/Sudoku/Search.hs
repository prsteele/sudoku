module Sudoku.Search where

import Control.Monad
import Control.Monad.Reader
import Data.List (minimumBy)

import Sudoku.Puzzle
import Sudoku.Rules

type Search = Reader Puzzle

search :: Contents -> Search [Contents]
search contents = do
  eligibility <- puzzleEligibility contents
  case eligibility of
    Complete   -> return [contents]
    Incomplete -> searchChildren contents
    Witness _  -> return []

searchChildren :: Contents -> Search [Contents]
searchChildren contents = do
  children <- nextCandidates contents
  results <- mapM search children

  return . concat . filter (not . null) $ results

safeMinimumOn :: Ord b => (a -> b) -> a -> [a] -> a
safeMinimumOn _    x [] = x
safeMinimumOn value _ xs = minimumBy comp xs
  where
    comp x y = compare (value x) (value y)

nextCandidates :: Contents -> Search [Contents]
nextCandidates contents = do
  puzzle <- ask
  let empty = puzzle `emptyCells` contents
      completions = cellCompletions puzzle contents <$> empty
  return $ safeMinimumOn length [] completions

puzzleEligibility :: Contents -> Search Status
puzzleEligibility contents = do
  puzzle <- ask
  return (puzzleStatus puzzle contents)

solvePuzzle :: Puzzle -> Contents -> [Contents]
solvePuzzle puzzle start
  = runReader (search start) puzzle
