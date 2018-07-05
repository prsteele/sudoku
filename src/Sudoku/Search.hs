{-# LANGUAGE TupleSections #-}
module Sudoku.Search where

import Control.Monad
import Control.Monad.Reader
import Data.List (sortOn)

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
  results <- forM children (uncurry (searchCell contents))

  -- We consider all completions of a cell in these results. If the
  -- results for the first cell are empty, then the results for all
  -- cells will be empty, and we need not compute those empty lists.
  return $ case results of
             (x: xs) -> if null x
                        then []
                        else concat (x: xs)
             []      -> []

searchCell :: Contents -> Cell -> [Value] -> Search [Contents]
searchCell contents cell values = do
  results <- mapM (search . fillCell contents cell) values
  return (concat results)

nextCandidates :: Contents -> Search [(Cell, [Value])]
nextCandidates contents =
  let
    cells puzzle = emptyCells puzzle contents
    completions puzzle = cellCompletions puzzle contents <$> (cells puzzle)
  in do
    puzzle <- ask
    let empty = puzzle `emptyCells` contents
        candidates = cellCandidates puzzle contents <$> empty
    return $ sortOn (length . snd) (zip empty candidates)

puzzleEligibility :: Contents -> Search Status
puzzleEligibility contents = do
  puzzle <- ask
  return (puzzleStatus puzzle contents)

solvePuzzle :: Puzzle -> Contents -> [Contents]
solvePuzzle puzzle start
  = runReader (search start) puzzle
